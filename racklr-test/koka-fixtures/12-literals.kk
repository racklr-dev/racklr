// Number, char and string literals, including digit separators, hex numbers,
// escapes, raw strings and a multi-line raw string.
module fixtures/literals

val decimal = 123456
val grouped = 1_000_000
val hexadecimal = 0x2A
val negative = 0 - 42
val fraction = 3.14159
val exponent = 1.0e10
val letter = 'a'
val newline-char = '\n'
val quoted = "a \"quoted\" word\twith a tab"
val raw-path = r"C:\temp\file.txt"
val raw-hash = r#"a "quoted" word"#
val multi-line = r"
first raw line
second raw line"
