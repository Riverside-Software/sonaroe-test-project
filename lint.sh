sonarlint -e -X -Dsonar.oe.propath=src/procedures -Dsonar.oe.databases="dump/sp2k.df:abc,dump/empty.df:newdb" -Dsonar.oe.aliases="abc,sp2k,foobar;newdb,db2" --src "{src/procedures/sample/test1.p,src/procedures/sample/test2.p}"

