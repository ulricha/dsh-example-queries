#!/bin/bash

for i in `seq 1 22` ; do
	mkdir native_tpch_${i}
	cp $HOME/work/dev/dsh-example-queries/native_queries/tpch_native/q${i}.sql native_tpch_${i}/native_tpch_q${i}_1.sql;
done
