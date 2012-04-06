@echo off
erlc -o ebin -I include src/erlsom.erl
erlc -o ebin -I include src/erlsom_add.erl
erlc -o ebin -I include src/erlsom_compile.erl
erlc -o ebin -I include src/erlsom_lib.erl
erlc -o ebin -I include src/erlsom_parse.erl
erlc -o ebin -I include src/erlsom_parseXsd.erl
erlc -o ebin -I include src/erlsom_pass2.erl
erlc -o ebin -I include src/erlsom_sax.erl
erlc -o ebin -I include src/erlsom_sax_latin1.erl
erlc -o ebin -I include src/erlsom_sax_lib.erl
erlc -o ebin -I include src/erlsom_sax_list.erl
erlc -o ebin -I include src/erlsom_sax_utf8.erl
erlc -o ebin -I include src/erlsom_sax_utf16be.erl
erlc -o ebin -I include src/erlsom_sax_utf16le.erl
erlc -o ebin -I include src/erlsom_simple_form.erl
erlc -o ebin -I include src/erlsom_ucs.erl
erlc -o ebin -I include src/erlsom_write.erl
erlc -o ebin -I include src/erlsom_writeHrl.erl
erlc -o ebin -I include src/ucs.erl
pause