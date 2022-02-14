MODULES=$(ls src/modules/ | grep -v '^mod\.rs$'| sed -e 's/\.rs$//g')

echo $MODULES | awk '{print "mod "$1";"}' >src/modules/mod.rs
echo "pub fn gen_modules() -> Vec<std::sync::Arc<dyn crate::mod_load::Module>> {vec![" >>src/modules/mod.rs
echo $MODULES | awk '{print "\tstd::sync::Arc::new("$1"::Handler),"}' >>src/modules/mod.rs
echo "]}" >>src/modules/mod.rs
