#!/bin/bash

tab=${1}
file=${2}

query="
DropTable('${tab}', TRUE);
CreateTable('${tab}', ['index' slng]);
PrimaryKey('${tab}_pkey', '${tab}', ['index']);
SortKey('${tab}_skey', '${tab}', ['index']);
Append('${tab}',
       Project(AsciiLoad(['p'], '${file}', ',', '\n'),
               [p=slng(p)]));
Commit;
"

echo "${query}"
