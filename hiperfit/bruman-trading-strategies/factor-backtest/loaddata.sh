#!/bin/bash

tab=${1}
file=${2}

query="
DropTable('${tab}', TRUE);
CreateTable('${tab}', ['r' slng, 'c' slng, 'd' dbl]);
PrimaryKey('${tab}_pkey', '${tab}', ['r', 'c']);
SortKey('${tab}_skey', '${tab}', ['r', 'c']);
NullOp(
Append('${tab}',
       Project(AsciiLoad(['r', 'c', 'd'], '${file}', ',', '\n'),
               [r=slng(r), c=dbl(c), d=dbl(d)])));
Commit;
"

echo "${query}"
