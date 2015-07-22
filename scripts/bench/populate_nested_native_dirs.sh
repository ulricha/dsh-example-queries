#!/bin/bash

for f in $HOME/work/dev/dsh-example-queries/native_queries/nested_native/*.sql ; do
	mkdir `basename ${f} .sql`
	cp ${f} `basename ${f} .sql`/`basename ${f}`;
done
