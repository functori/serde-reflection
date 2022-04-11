use crate::{
    indent::{IndentConfig, IndentedWriter},
    CodeGeneratorConfig,
};
use heck::SnakeCase;
use include_dir::include_dir as include_directory;
use serde_reflection::{ContainerFormat, Format, Named, Registry, VariantFormat};
use std::{
    collections::{BTreeMap},
    io::{Result, Write},
    path::PathBuf,
};

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

pub struct CodeGenerator<'a> {
    config: &'a CodeGeneratorConfig,
    libraries: Vec<String>,
}

struct OCamlEmitter<'a, T> {
    out: IndentedWriter<T>,
    generator: &'a CodeGenerator<'a>,
}

impl<'a> CodeGenerator<'a> {

    pub fn new(config: &'a CodeGeneratorConfig) -> Self {
        if config.c_style_enums {
            panic!("OCaml does not support generating c-style enums");
        }
        Self {
            config,
            libraries: config.external_definitions.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        }
    }

    pub fn output(&self, out: &mut dyn Write, registry: &Registry) -> Result<()> {
        let mut emitter = OCamlEmitter {
            out: IndentedWriter::new(out, IndentConfig::Space(2)),
            generator: self,
        };
        emitter.output_preamble()?;
        // todo: do a strongly connected components sort to regroup types that are cyclic
        let mut i = 0;
        let n = registry.len();
        for (name, format) in registry {
            let first = i == 0;
            let last = i == n-1;
            emitter.output_container(name, format, first, last)?;
            i = i + 1;
        }
        Ok(())
    }
}

impl<'a, T> OCamlEmitter<'a, T>
where T: Write, {

    fn output_preamble(&mut self) -> Result<()> {
        for namespace in self.generator.libraries.iter() {
            writeln!(self.out, "open {}\n", capitalize(namespace))?;
        }
        Ok(())
    }

    fn type_name(&self, s: String) -> String {
        let protected_keywords : [String; 66]  = [
            "and".to_string(), "as".to_string(), "assert".to_string(), "asr".to_string(),
            "begin".to_string(), "class".to_string(), "constraint".to_string(),
            "do".to_string(), "done".to_string(), "downto".to_string(), "else".to_string(),
            "end".to_string(), "exception".to_string(), "external".to_string(),
            "false".to_string(), "for".to_string(), "fun".to_string(), "function".to_string(),
            "functor".to_string(), "if".to_string(), "in".to_string(), "include".to_string(),
            "inherit".to_string(), "initializer".to_string(), "land".to_string(),
            "lazy".to_string(), "let".to_string(), "lor".to_string(), "lsl".to_string(),
            "lsr".to_string(), "lxor".to_string(), "match".to_string(), "method".to_string(),
            "mod".to_string(), "module".to_string(), "mutable".to_string(), "new".to_string(),
            "nonrec".to_string(), "object".to_string(), "of".to_string(), "open".to_string(),
            "or".to_string(), "private".to_string(), "rec".to_string(), "sig".to_string(),
            "struct".to_string(), "then".to_string(), "to".to_string(), "true".to_string(),
            "try".to_string(), "type".to_string(), "val".to_string(), "virtual".to_string(),
            "when".to_string(), "while".to_string(), "with".to_string(), "bool".to_string(),
            "string".to_string(), "bytes".to_string(), "char".to_string(), "unit".to_string(),
            "option".to_string(), "float".to_string(), "list".to_string(),
            "int32".to_string(), "int64".to_string()
        ];

        let s = s.to_snake_case();
        if protected_keywords.contains(&s) { s + "_" }
        else { s }
    }

    fn format(&self, format: &Format) -> String {
        use Format::*;
        match format {
            Variable(_) => panic!("incorrect value"),
            TypeName(s) => self.type_name(s.to_string()),
            Unit => "unit".into(),
            Bool => "bool".into(),
            I8 => "Stdint.int8".into(),
            I16 => "Stdint.int16".into(),
            I32 => "int32".into(),
            I64 => "int64".into(),
            I128 => "Stdint.int128".into(),
            U8 => "Stdint.uint8".into(),
            U16 => "Stdint.uint16".into(),
            U32 => "Stdint.uint32".into(),
            U64 => "Stdint.uint64".into(),
            U128 => "Stdint.uint128".into(),
            F32 => "float".into(),
            F64 => "float".into(),
            Char => "char".into(),
            Str => "string".into(),
            Bytes => "bytes".into(),
            Option(f) => format!("{} option", self.format(f)),
            Seq(f) => format!("{} list", self.format(f)),
            Map{key, value} => self.map_format(key, value),
            Tuple(fs) => self.tuple_format(fs),
            TupleArray{content, size} => self.tuple_format(&vec![content.as_ref().clone(); *size]),
        }
    }

    fn map_format(&self, key: &Format, value: &Format) -> String {
        format!("({}, {}) Serde.map", self.format(key), self.format(value))
    }

    fn tuple_format(&self, formats: &Vec<Format>) -> String {
        format!("({})", formats.iter().map(|f| self.format(f)).collect::<Vec<_>>().join(" * "))
    }

    fn record_format(&self, formats: &Vec<Named<Format>>, inside_variant: bool) -> String {
        let indent1 = if inside_variant { "      " } else  { "  " };
        let indent2 = if inside_variant { "    " } else  { "" };
        format!("{{\n{}\n{}}}", formats.iter().map(|f| format!("{}{}: {}", indent1, f.name, self.format(&f.value))).collect::<Vec<_>>().join(";\n"), indent2)
    }

    fn variant_format(&self, format: &VariantFormat) -> String {
        use VariantFormat::*;
        match format {
            Variable(_) => panic!("incorrect value"),
            Unit => "".to_string(),
            NewType(f) => format!(" of {}", self.format(f)),
            Tuple(fields) => {
                if fields.len() == 0 { "".to_string() }
                else { format!(" of {}", self.tuple_format(fields)) }
            },
            Struct(fields) => {
                if fields.len() == 0 { "".to_string() }
                else { format!(" of {}",self.record_format(fields, true)) }
            }
        }
    }

    fn enum_format(&self, formats: &BTreeMap<u32, Named<VariantFormat>>) -> String {
        formats.iter().map(|(_, f)| format!("  | {}{}", f.name, self.variant_format(&f.value))).collect::<Vec<_>>().join("\n")
    }

    fn serialize(&self) -> String {
        if self.generator.config.serialization { "[@@deriving serde]".to_string() }
        else { "".to_string() }
    }

    fn output_type(&mut self, name: &str, s: String, variant: bool, first: bool, last: bool) -> Result<()> {
        let decl = if first { "type" } else { "and" };
        let serialize = if last { self.serialize() } else { "".to_string() };
        let sep = if variant { "\n".to_string() } else { " ".to_string() };
        writeln!(self.out, "{} {} = {}{}{}{}\n", decl, self.type_name(name.to_string()), sep, s, sep, serialize)
    }

    fn is_cyclic(&self, name: &str, format: &Format) -> bool {
        use Format::*;
        match format {
            TypeName(s) => name == s,
            Option(f) => self.is_cyclic(name, f),
            Seq(f) => self.is_cyclic(name, f),
            Map{key, value} => self.is_cyclic(name, key) || self.is_cyclic(name, value),
            Tuple(fs) => fs.iter().any(|f| self.is_cyclic(name, f)),
            TupleArray{content, size: _} => self.is_cyclic(name, content),
            _ => false
        }
    }

    fn output_container(&mut self, name: &str, format: &ContainerFormat, first: bool, last: bool) -> Result<()> {
        use ContainerFormat::*;
        match format {
            UnitStruct => self.output_type(name, "unit".to_string(), false, first, last),
            NewTypeStruct(format) => {
                if self.is_cyclic(name, format.as_ref()) {
                    let mut map = BTreeMap::new();
                    map.insert(0, Named { name: format!("{}_", capitalize(name)), value: VariantFormat::NewType(format.clone()) });
                    self.output_type(name, self.enum_format(&map), true, first, last)
                } else {
                    self.output_type(name, self.format(format.as_ref()), false, first, last)
                }
            },
            TupleStruct(formats) => self.output_type(name, self.tuple_format(formats), false, first, last),
            Struct(fields) => self.output_type(name, self.record_format(fields, false), false, first, last),
            Enum(variants) => self.output_type(name, self.enum_format(variants), true, first, last),
        }
    }
}

pub struct Installer {
    install_dir: PathBuf,
}

impl Installer {
    pub fn new(install_dir: PathBuf) -> Self {
        Installer { install_dir }
    }

    fn install_runtime(
        &self,
        source_dir: include_dir::Dir,
        path: &str,
    ) -> std::result::Result<(), Box<dyn std::error::Error>> {
        let dir_path = self.install_dir.join(path);
        std::fs::create_dir_all(&dir_path)?;
        for entry in source_dir.files() {
            let mut file = std::fs::File::create(dir_path.join(entry.path()))?;
            file.write_all(entry.contents())?;
        }
        Ok(())
    }
}

impl crate::SourceInstaller for Installer {
    type Error = Box<dyn std::error::Error>;

    fn install_module(
        &self,
        config: &CodeGeneratorConfig,
        registry: &Registry,
    ) -> std::result::Result<(), Self::Error> {
        let dir_path = self.install_dir.join(&config.module_name);
        std::fs::create_dir_all(&dir_path)?;
        let dune_project_source_path = self.install_dir.join("dune-project");
        let mut dune_project_file = std::fs::File::create(dune_project_source_path)?;
        writeln!(dune_project_file, "(lang dune 3.0)")?;
        let dune_source_path = dir_path.join("dune");
        let mut dune_file = std::fs::File::create(dune_source_path)?;
        let name = config.module_name.to_snake_case();
        writeln!(dune_file, "(library\n (name {0})\n (modules {0})\n (preprocess (pps ppx)))", name)?;
        let source_path = dir_path.join(format!("{}.ml", name));
        let mut file = std::fs::File::create(source_path)?;
        let generator = CodeGenerator::new(config);
        generator.output(&mut file, registry)?;
        Ok(())
    }

    fn install_serde_runtime(&self) -> std::result::Result<(), Self::Error> {
        self.install_runtime(include_directory!("runtime/ocaml/common"), "common")?;
        self.install_runtime(include_directory!("runtime/ocaml/virtual"), "virtual")?;
        self.install_runtime(include_directory!("runtime/ocaml/ppx"), "ppx")?;
        self.install_runtime(include_directory!("runtime/ocaml/serde"), "serde")
    }

    fn install_bincode_runtime(&self) -> std::result::Result<(), Self::Error> {
        self.install_runtime(include_directory!("runtime/ocaml/common"), "common")?;
        self.install_runtime(include_directory!("runtime/ocaml/virtual"), "virtual")?;
        self.install_runtime(include_directory!("runtime/ocaml/ppx"), "ppx")?;
        self.install_runtime(include_directory!("runtime/ocaml/serde"), "serde")?;
        self.install_runtime(include_directory!("runtime/ocaml/bincode"), "bincode")
    }

    fn install_bcs_runtime(&self) -> std::result::Result<(), Self::Error> {
        self.install_runtime(include_directory!("runtime/ocaml/common"), "common")?;
        self.install_runtime(include_directory!("runtime/ocaml/virtual"), "virtual")?;
        self.install_runtime(include_directory!("runtime/ocaml/ppx"), "ppx")?;
        self.install_runtime(include_directory!("runtime/ocaml/serde"), "serde")?;
        self.install_runtime(include_directory!("runtime/ocaml/bcs"), "bcs")
    }
}
