#[cfg(test)]
pub fn test_success(program: &str, exit_code: i32, stdout: &str) {
    test(program, 1024, 0, exit_code, stdout, Some(""));
}

#[cfg(test)]
pub fn test_gc(program: &str, heap_size: usize, exit_code: i32, stdout: &str) {
    test(program, heap_size, 0, exit_code, stdout, None);
}

#[cfg(test)]
pub fn test_error(
    program: &str,
    compiler_exit_code: i32,
    exit_code: i32,
    stdout: &str,
    stderr: &str,
) {
    test(
        program,
        1024,
        compiler_exit_code,
        exit_code,
        stdout,
        Some(stderr),
    );
}

#[cfg(test)]
pub fn test(
    program: &str,
    heap_size: usize,
    compiler_exit_code: i32,
    exit_code: i32,
    stdout: &str,
    stderr: Option<&str>,
) {
    use crate::compile;
    use std::path::PathBuf;
    use std::process::Command;
    use std::str;
    use tempfile::tempdir;

    let dir = tempdir().unwrap();
    let mut file = PathBuf::from(dir.path());
    file.push(format!("source.diamondback"));

    let mut output = PathBuf::from(dir.path());
    output.push("test.out");

    match compile(
        program,
        &file.to_str().unwrap(),
        output.to_str().unwrap(),
        false,
        false,
        None,
    ) {
        Ok(binary) => {
            assert_eq!(
                compiler_exit_code, 0,
                "Compiler exited with 0, expected error"
            );
            let output = Command::new(binary)
                .env("HEAP_SIZE", format!("{}", heap_size))
                .output()
                .expect("Failed to run");

            let stderr_actual = str::from_utf8(&output.stderr).unwrap();
            if (stderr.is_none() && !stderr_actual.is_empty())
                || (stderr.is_some() && stderr.unwrap() != stderr_actual)
            {
                println!("{}", stderr_actual);
            }

            if let Some(stderr) = stderr {
                assert_eq!(stderr_actual, stderr);
            }

            if output.status.code().is_none() {
                assert!(false, "Program segfaulted!");
            }

            assert_eq!(output.status.code(), Some(exit_code), "program exit code");
            assert_eq!(str::from_utf8(&output.stdout).unwrap(), stdout);
        }
        Err(code) => {
            assert_eq!(compiler_exit_code, code, "compiler exit code");
        }
    }
}
