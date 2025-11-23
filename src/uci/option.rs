use std::fs;

pub trait UciOption {
    fn name(&self) -> &str;
    fn type_name(&self) -> &'static str;
    fn print(&self);
    fn set_value_from_str(&mut self, value: &str) -> anyhow::Result<()>;

    fn val_bool(&self) -> bool;
    fn val_string(&self) -> &str;
}

pub type FilePathString = String;

pub struct UciOptionType<T>
where
    T: Clone + ToString,
{
    name: String,
    value: T,
    default: T,
}

impl<T> UciOptionType<T>
where
    T: Clone + ToString,
{
    pub fn new(name: &str, default: T) -> Self {
        Self {
            name: name.to_string(),
            value: default.clone(),
            default,
        }
    }
}

impl UciOption for UciOptionType<bool> {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_name(&self) -> &'static str {
        "check"
    }

    fn set_value_from_str(&mut self, value: &str) -> anyhow::Result<()> {
        match value {
            "true" => self.value = true,
            "false" => self.value = false,
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid value for option {}: {}",
                    self.name,
                    value
                ));
            }
        }

        Ok(())
    }

    fn print(&self) {
        println!(
            "option name {} type {} default {}",
            self.name,
            self.type_name(),
            if self.default { "true" } else { "false" }
        );
    }

    fn val_bool(&self) -> bool {
        self.value
    }

    fn val_string(&self) -> &str {
        panic!("Called val_string on a non-string (bool) UCI option");
    }
}

impl UciOption for UciOptionType<FilePathString> {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_name(&self) -> &'static str {
        "string"
    }

    fn set_value_from_str(&mut self, value: &str) -> anyhow::Result<()> {
        match fs::exists(value) {
            Ok(true) => self.value = value.to_string(),
            Ok(false) => {
                return Err(anyhow::anyhow!(
                    "File path \"{}\" for option {} does not exist",
                    value,
                    self.name,
                ));
            }
            Err(err) => {
                return Err(anyhow::anyhow!(
                    "Error accessing file path \"{}\" for option {}: {}",
                    value,
                    self.name,
                    err
                ));
            }
        }

        Ok(())
    }

    fn print(&self) {
        println!(
            "option name {} type {} default {}",
            self.name,
            self.type_name(),
            self.default,
        );
    }

    fn val_bool(&self) -> bool {
        panic!("Called val_bool on a non-bool (FilePathString) UCI option");
    }

    fn val_string(&self) -> &str {
        &self.value
    }
}
