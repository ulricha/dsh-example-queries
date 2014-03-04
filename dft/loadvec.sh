#!/bin/bash

tab=${1}
file=${2}

query="
DropTable('${tab}', TRUE);
CreateTable('${tab}', ['pos' dbl, 'valr' dbl, 'vali' dbl]);
PrimaryKey('${tab}_pkey', '${tab}', ['pos']);
SortKey('${tab}_skey', '${tab}', ['pos']);
Append('${tab}',
       Project(AsciiLoad(['pos', 'valr', 'vali'], '${file}', ',', '\n'),
               [pos=slng(pos), valr=dbl(valr), vali=dbl(vali)]));
Commit;
"

echo "${query}"
