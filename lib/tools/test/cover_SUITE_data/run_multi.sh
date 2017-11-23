erlc -o ebin src/cover.erl
erlc -o test/cover_SUITE_data/ test/cover_SUITE_data/run_multi.erl
erl -pa test/cover_SUITE_data/ -s run_multi run multi_before.html -s init stop
erl -pa ebin -pa test/cover_SUITE_data/ -s run_multi run multi_after.html -s init stop
rm test/cover_SUITE_data/run_multi.beam
