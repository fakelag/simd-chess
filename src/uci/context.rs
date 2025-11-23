use crate::uci::option::{self, UciOption};

pub struct UciContextInner<OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    options: Vec<(OID, Box<dyn option::UciOption + Send + Sync>)>,
}

pub struct UciContext<OID>(std::sync::Arc<std::sync::Mutex<UciContextInner<OID>>>)
where
    OID: std::cmp::PartialEq + std::fmt::Debug;

impl<OID> UciContext<OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    pub fn new() -> Self {
        Self(std::sync::Arc::new(std::sync::Mutex::new(
            UciContextInner::new(),
        )))
    }

    pub fn lock_add(&self, id: OID, v: Box<dyn option::UciOption + Send + Sync>) {
        let mut context = self.0.lock().unwrap();
        context.add(id, v);
    }

    pub fn lock(&self) -> std::sync::MutexGuard<'_, UciContextInner<OID>> {
        self.0.lock().unwrap()
    }
}

impl<OID> Clone for UciContext<OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    fn clone(&self) -> Self {
        UciContext(self.0.clone())
    }
}

pub struct UciOptionIterator<'a, OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    index: usize,
    context: &'a UciContextInner<OID>,
}

impl<'a, OID> Iterator for UciOptionIterator<'a, OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    type Item = &'a dyn UciOption;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.context.options.len() {
            return None;
        }

        let option = &self.context.options[self.index];
        self.index += 1;

        Some(option.1.as_ref())
    }
}

impl<OID> UciContextInner<OID>
where
    OID: std::cmp::PartialEq + std::fmt::Debug,
{
    pub fn new() -> Self {
        Self {
            options: Vec::new(),
        }
    }

    pub fn add(&mut self, id: OID, v: Box<dyn UciOption + Send + Sync>) {
        if self
            .options
            .iter()
            .any(|(existing_id, _)| existing_id == &id)
        {
            panic!("Option ID {:?} already exists", id);
        }

        self.options.push((id, v));
    }

    pub fn get_by_id(&self, id: OID) -> &dyn UciOption {
        let option = self
            .options
            .iter()
            .find(|(existing_id, _)| existing_id == &id)
            .expect(&format!("Option ID {:?} not found", id));

        option.1.as_ref()
    }

    pub fn get_by_id_mut(&mut self, id: OID) -> &mut dyn UciOption {
        let option = self
            .options
            .iter_mut()
            .find(|(existing_id, _)| existing_id == &id)
            .expect(&format!("Option ID {:?} not found", id));

        option.1.as_mut()
    }

    pub fn get_by_name(&self, name: &str) -> Option<&Box<dyn UciOption + Send + Sync>> {
        let option = self.options.iter().find(|(_, opt)| opt.name() == name);
        option.map(|(_, opt)| opt)
    }

    pub fn get_by_name_mut(&mut self, name: &str) -> Option<&mut Box<dyn UciOption + Send + Sync>> {
        let option = self.options.iter_mut().find(|(_, opt)| opt.name() == name);
        option.map(|(_, option)| option)
    }

    pub fn iter<'a>(&'a self) -> UciOptionIterator<'a, OID> {
        UciOptionIterator {
            index: 0,
            context: self,
        }
    }
}
