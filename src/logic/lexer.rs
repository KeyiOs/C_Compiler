use crate::Token;
use crate::data::maps::{ DOUBLE_OPERATOR_MAP, KEYWORD_MAP, SINGLE_OPERATOR_MAP, TRIPLE_OPERATOR_MAP };
use crate::data::TokenType;
use std::iter::Peekable;
use std::str::Chars;

pub fn lexer_start(source: &str) -> Result<Vec<Token>, Box<dyn std::error::Error>> {
    /**/ /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */ /**/
    /**/ /*                    Lexer State Variables                    */ /**/
    /**/ /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */ /**/
    /**/ let mut token: Vec<Token> = Vec::new();                           /**/
    /**/ let mut chars: Peekable<Chars<'_>> = source.chars().peekable();   /**/
    /**/ let mut buffer: String = String::new();                           /**/
    /**/ let mut start_of_line: bool = true;                               /**/
    /**/ let mut has_decimal: bool = false;                                /**/
    /**/ let mut line: u16 = 1;                                            /**/
    /**/ let mut filename: String = String::new();                         /**/
    /**/ /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */ /**/
    

    while let Some(character) = chars.next() {
        if character.is_ascii_alphabetic() || character == '_' {
            buffer.push(character);

            while let Some(&next_char) = chars.peek() {
                if !next_char.is_ascii_alphanumeric() && next_char != '_' {
                    break;
                }

                buffer.push(chars.next().unwrap());
            }

            if KEYWORD_MAP.contains_key(buffer.as_str()) {
                token.push(Token::new(TokenType::Keyword(buffer.clone()), line));
            } else {
                token.push(Token::new(TokenType::Identifier(buffer.clone()), line));
            }
            
            buffer.clear();
        } else if character.is_ascii_digit() || ((character == '.' || character == '-') && chars.peek().map_or(false, |c| c.is_ascii_digit())) {
            match (character, chars.peek()) {
                ('0', Some(&next_char)) if is_octal(next_char) => {
                    let mut oct_digits = String::new();

                    while let Some(&c) = chars.peek() {
                        if !is_octal(c) {
                            break;
                        }

                        chars.next();
                        oct_digits.push(c);
                    }

                    let oct_value = u32::from_str_radix(&oct_digits, 8).map_err(|_| format!("Invalid octal number '{}' on line {}", oct_digits, line))?;

                    token.push(Token::new(TokenType::Literal(oct_value.to_string(), "number".to_string()), line));
                    start_of_line = false;

                    continue;
                } ('0', Some(&next_char)) if next_char == 'x' || next_char == 'X' => {
                    let mut hex_digits = String::new();

                    chars.next();
                    while let Some(&c) = chars.peek() {
                        if !c.is_ascii_hexdigit() {
                            break;
                        }

                        hex_digits.push(c);
                        chars.next();
                    }

                    if hex_digits.is_empty() {
                        return Err(format!("{}, {}: Invalid hex number", filename, line).into());
                    }

                    let hex_value = u32::from_str_radix(&hex_digits, 16).map_err(|_| format!("Invalid hex number '{}' on line {}", hex_digits, line))?;

                    token.push(Token::new(TokenType::Literal(hex_value.to_string(), "number".to_string()), line));
                    start_of_line = false;

                    continue;
                } _ => {}
            }

            if character == '.' {
                buffer.push('0');
                has_decimal = true;
            }

            buffer.push(character);

            while let Some(&next_char) = chars.peek() {
                if next_char.is_ascii_digit() {
                    buffer.push(chars.next().unwrap());
                } else if next_char == '.' {
                    if has_decimal {
                        return Err(format!("{}, {}: Multiple decimal points in number", filename, line).into());
                    }

                    has_decimal = true;
                    buffer.push(chars.next().unwrap());
                } else if next_char == 'e' || next_char == 'E' {
                    buffer.push(chars.next().unwrap());

                    if matches!(chars.peek(), Some(&c) if c == '+' || c == '-') {
                        buffer.push(chars.next().unwrap());
                    }

                    let buffer_length = buffer.len();
                    while let Some(&exp_digit) = chars.peek() {
                        if !exp_digit.is_ascii_digit() {
                            break;
                        }

                        buffer.push(chars.next().unwrap());
                    }

                    if buffer_length == buffer.len() {
                        return Err(format!("{}, {}: Invalid exponent in number", filename, line).into());
                    }
                } else if next_char.is_ascii_alphabetic() {
                    let mut suffix = String::new();

                    for _ in 0..3 {
                        let Some(&c) = chars.peek() else {
                            break;
                        };

                        let c = c.to_ascii_lowercase();

                        if c != 'u' && c != 'l' && c != 'f' {
                            break;
                        }

                        suffix.push(chars.next().unwrap());
                    }

                    if let Some(&next) = chars.peek() {
                        if !next.is_whitespace() && !SINGLE_OPERATOR_MAP.contains_key(&next) {
                            return Err(format!("{}, {}: Invalid number suffix", filename, line).into());
                        }
                    }

                    if !matches!(suffix.to_ascii_lowercase().as_str(), "u" | "l" | "f" | "ul" | "lu" | "ll" | "ull" | "llu" | "lf") {
                        return Err(format!("{}, {}: Invalid number suffix", filename, line).into());
                    }

                    buffer.push_str(&suffix);
                } else {
                    break;
                }
            }

            token.push(Token::new(TokenType::Literal(buffer.clone(), "number".to_string()), line));
            has_decimal = false;
            
            buffer.clear();
        } else if character.is_whitespace() {
            if character == '\n' {
                line += 1;
                start_of_line = true;
            } else {
                start_of_line = false;
            }

            continue;
        } else if SINGLE_OPERATOR_MAP.contains_key(&character) {
            if character == '\'' {
                let Some(character) = chars.next() else {
                    return Err(format!("{}, {}: Unterminated character literal", filename, line).into());
                };

                if character == '\'' {
                    return Err(format!("{}, {}: Empty character literal", filename, line).into());
                } else if character == '\\' {
                    if chars.peek().is_none() {
                        return Err(format!("{}, {}: Unterminated character literal", filename, line).into());
                    };

                    let char_literal = process_escape_sequence(&mut chars, line, &filename)?;

                    token.push(Token::new(TokenType::Literal(char_literal, "char".to_string()), line));
                } else {    
                    token.push(Token::new(TokenType::Literal(character.to_string(), "char".to_string()), line));
                }

                if chars.next() != Some('\'') {
                    while let Some(next_char) = chars.next() {
                        if next_char == '\n' {
                            return Err(format!("{}, {}: Unterminated character literal", filename, line).into());
                        } else if next_char == '\'' {
                            return Err(format!("{}, {}: Character literal too long", filename, line).into());
                        }
                    }

                    return Err(format!("{}, {}: Unterminated character literal", filename, line).into());
                }
            } else if character == '\"' {
                let mut string_lit = String::new();
                let mut ok = false;

                while let Some(next_char) = chars.next() {
                    match next_char {
                        '"' => {
                            let mut temp_iter = chars.clone();
                            let mut skip_count = 0;
                            let mut found_quote = false;

                            while let Some(&c) = temp_iter.peek() {
                                if !c.is_whitespace() {
                                    if c == '"' {
                                        found_quote = true;
                                    }
                                    break;
                                }
                                skip_count += 1;
                                temp_iter.next();
                            }

                            if !found_quote {
                                token.push(Token::new(TokenType::Literal(string_lit, "string".to_string()), line));
                                ok = true;
                                break;
                            }

                            for _ in 0..skip_count {
                                chars.next();
                            }
                            chars.next();

                            continue;
                        } '\\' => {
                            let Some(&escape_char) = chars.peek() else {
                                return Err(format!("{}, {}: Unterminated string literal", filename, line).into());
                            };

                            if escape_char == '\n' || escape_char == '\r' {
                                chars.next();
                                chars.next();

                                line += 1;

                                continue;
                            }

                            process_escape_sequence(&mut chars, line, &filename)?;
                        }

                        '\n' => return Err(format!("{}, {}: Unterminated string literal", filename, line).into()),

                        _ => string_lit.push(next_char),
                    }
                }

                if !ok {
                    return Err(format!("{}, {}: Unterminated string literal", filename, line).into());
                }
            } else if let Some(second_char) = chars.peek().copied() {
                let mut lookahead = chars.clone();
                lookahead.next();

                if let Some(third_char) = lookahead.peek() {
                    let triple_symbol = format!("{}{}{}", character, second_char, third_char);

                    if TRIPLE_OPERATOR_MAP.contains_key(triple_symbol.as_str()) {
                        for _ in 0..2 {
                            chars.next();
                        }

                        token.push(Token::new(TokenType::Operator(triple_symbol), line));
                        start_of_line = false;

                        continue;
                    }
                }

                let double_symbol = format!("{}{}", character, second_char);

                if DOUBLE_OPERATOR_MAP.contains_key(double_symbol.as_str()) {
                    token.push(Token::new(TokenType::Operator(double_symbol), line));
                    chars.next();
                } else {
                    token.push(Token::new(TokenType::Operator(character.to_string()), line));
                }
            } else {
                token.push(Token::new(TokenType::Operator(character.to_string()), line));
            }
        } else if character == '#' && start_of_line {
            let mut pp_line_num = String::new();

            while let Some(c) = chars.next() {
                if c.is_numeric() {
                    pp_line_num.push(c);

                    if !chars.peek().map_or(false, |c| c.is_ascii_digit()) {
                        line = pp_line_num.parse::<u16>().unwrap_or(0);
                    }
                } else if c == '"' {
                    let mut pp_filename = String::new();

                    while let Some(fc) = chars.next() {
                        if fc == '"' {
                            break;
                        }

                        pp_filename.push(fc);
                    }

                    filename = pp_filename.split('/').last().unwrap().to_string();
                } else if c == '\n' || c == '\r' {
                    break;
                }
            }

            while let Some(&c) = chars.peek() {
                if c != '\n' && c != '\r' {
                    break;
                }

                chars.next();
            }

            continue;
        } else {
            return Err(format!("{}, {}: Unknown character: {}", filename, line, character).into());
        }

        start_of_line = false;
    }

    token.push(Token::new(TokenType::EOF, line));

    token.reverse();
    
    Ok(token)
}


fn is_octal(c: char) -> bool {
    c >= '0' && c <= '7'
}


fn process_escape_sequence(chars: &mut Peekable<Chars<'_>>, line: u16, filename: &String) -> Result<String, Box<dyn std::error::Error>> {
    let Some(escape_char) = chars.next() else {
        return Err(format!("{}, {}: Error 001", filename, line).into());
    };

    let mut string_lit = String::new();

    match escape_char {
        'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' | '"' => {
            let escaped_char = match escape_char {
                'a' => '\x07',
                'b' => '\x08',
                'f' => '\x0C',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'v' => '\x0B',
                '\\' => '\\',
                '\'' => '\'',
                '"'  => '"',
                _ => unreachable!(),
            };

            string_lit.push(escaped_char);
            Ok(string_lit)
        }
        
        'x' => {
            let mut hex_digits = String::new();

            for _ in 0..2 {
                let Some(&c) = chars.peek() else {
                    break;
                };

                if !c.is_ascii_hexdigit() {
                    break;
                }

                hex_digits.push(c);
                chars.next();
            }

            if hex_digits.is_empty() {
                return Err(format!("{}, {}: Expected hex digits after \\x", filename, line).into());
            }

            let value = u8::from_str_radix(&hex_digits, 16).map_err(|_| format!("Invalid hex escape: \\x{} on line {}", hex_digits, line))?;
            
            string_lit.push(value as char);
            Ok(string_lit)
        }

        'u' => {
            let mut unicode_digits = String::new();

            for _ in 0..4 {
                let Some(&c) = chars.peek() else {
                    return Err(format!("{}, {}: Invalid escape sequence. Requires 4 hex digits, but found {}", filename, line, unicode_digits.len()).into());
                };

                if c.is_ascii_hexdigit() {
                    unicode_digits.push(c);
                    chars.next();
                } else if c == '\'' {
                    return Err(format!("{}, {}: Invalid escape sequence. Requires 4 hex digits, but found {}", filename, line, unicode_digits.len()).into());
                } else {
                    return Err(format!("{}, {}: Invalid character '{}' in unicode escape", filename, line, c).into());
                }
            }

            if unicode_digits.is_empty() {
                return Err(format!("{}, {}: Empty unicode escape", filename, line).into());
            }

            let codepoint = u32::from_str_radix(&unicode_digits, 16).map_err(|_| format!("Invalid unicode escape: \\u{{{}}} on line {}", unicode_digits, line))?;

            let Some(ch) = char::from_u32(codepoint) else {
                return Err(format!("{}, {}: Invalid unicode codepoint: \\u{{{}}}", filename, line, unicode_digits).into());
            };

            string_lit.push(ch);
            Ok(string_lit)
        }

        '0'..='7' => {
            let oct_digits = format!("{}{}", escape_char, process_octal(chars));
            let oct_value = u8::from_str_radix(&oct_digits, 8).map_err(|_| format!("Invalid octal escape: \\{} on line {}", oct_digits, line))?;
            
            string_lit.push(oct_value as char);
            Ok(string_lit)
        }
        
        _ => return Err(format!("{}, {}: Invalid escape sequence: \\{}", filename, line, escape_char).into()),
    }
}


fn process_octal(chars: &mut Peekable<Chars<'_>>) -> String {
    let mut oct_digits = String::new();

    for _ in 0..2 {
        let Some(&c) = chars.peek() else {
            break;
        };

        if !is_octal(c) {
            break;
        }

        chars.next();
        oct_digits.push(c);
    }

    oct_digits
}