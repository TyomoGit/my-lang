use std::{
    collections::BTreeMap,
    fmt::{Display, Write},
    path::Path,
    str::Lines,
};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Scan,
    Parse,
    RunTime,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait Error: std::error::Error {
    fn position(&self) -> Position;
    fn kind() -> ErrorKind;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl Position {
    pub fn new(row: usize, column: usize) -> Self {
        Self { row, column }
    }
}

pub fn fmt_errors<T: Error>(
    source_lines: Lines,
    errors: &Vec<T>,
    file_path: Option<&Path>,
) -> String {
    let mut result = String::new();
    let mut line_error_map: BTreeMap<usize, Vec<&T>> = BTreeMap::new();

    for error in errors {
        let Position { row, column: _ } = error.position();
        line_error_map.entry(row).or_default().push(error);
    }

    for (row, errors) in line_error_map {
        for error in errors {
            writeln!(&mut result, "{}", error_details(error, file_path)).unwrap();
        }

        writeln!(&mut result, "{}\n", source_line(source_lines.clone(), row)).unwrap();
    }

    result
}

fn error_details<E: Error>(error: &E, file_path: Option<&Path>) -> String {
    let Position { row, column } = error.position();
    let (row, column) = (row.to_string(), column.to_string());
    let file_path =
        file_path.map_or_else(|| "<repl>".to_string(), |path| path.display().to_string());
    let mut result = String::new();
    writeln!(&mut result, "[🛑 {}Error] {}", E::kind(), error).unwrap();
    write!(&mut result, "  --> {}:{}:{}", file_path, row, column).unwrap();

    result
}

fn source_line(mut source_lines: Lines, row: usize) -> String {
    let source_code = source_lines.nth(row - 1).unwrap();
    let row = row.to_string();
    let padding_left = row.len() + 1;
    let mut result = String::new();
    writeln!(&mut result, "{}|", " ".repeat(padding_left)).unwrap();
    writeln!(&mut result, "{} | {}", row, source_code).unwrap();
    write!(&mut result, "{}|", " ".repeat(padding_left)).unwrap();

    result
}
