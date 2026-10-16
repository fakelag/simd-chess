use std::io::{BufRead, Write};

/// EOF or a read error counts as "no", so an unattended run (stdin from NUL) never proceeds on a guess.
pub fn confirm(question: &str) -> bool {
    print!("{question} [y/n] ");
    let _ = std::io::stdout().flush();

    let yes = answer_is_yes(&mut std::io::stdin().lock());
    if !yes {
        println!();
    }
    yes
}

fn answer_is_yes(input: &mut impl BufRead) -> bool {
    let mut answer = String::new();
    match input.read_line(&mut answer) {
        Ok(n) if n > 0 => matches!(answer.trim().to_ascii_lowercase().as_str(), "y" | "yes"),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_only_yes_confirms() {
        for (input, yes) in [
            ("y\n", true),
            ("Y\r\n", true),
            (" yes \n", true),
            ("YES", true),
            ("n\n", false),
            ("no\n", false),
            ("yep\n", false),
            ("\n", false),
            ("", false),
        ] {
            assert_eq!(answer_is_yes(&mut input.as_bytes()), yes, "{:?}", input);
        }
    }

    #[test]
    fn test_read_error_is_no() {
        struct Broken;
        impl std::io::Read for Broken {
            fn read(&mut self, _: &mut [u8]) -> std::io::Result<usize> {
                Err(std::io::Error::other("broken pipe"))
            }
        }
        assert!(!answer_is_yes(&mut std::io::BufReader::new(Broken)));
    }
}
