mod asyl;
pub fn gen_modules() -> Vec<std::sync::Arc<dyn crate::mod_load::Module>> {vec![
	std::sync::Arc::new(asyl::Handler),
]}
