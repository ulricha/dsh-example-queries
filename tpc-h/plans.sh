rm q*.pdf

for p in q*_vl.plan; do
	.cabal-sandbox/bin/vldot -i ${p} | dot -Tpdf -o `basename ${p} .plan`.pdf;
done

for p in q*_vl_opt.plan; do
	.cabal-sandbox/bin/vldot -i ${p} | dot -Tpdf -o `basename ${p} .plan`.pdf;
done

for p in q*_opt_ta.plan; do
	.cabal-sandbox/bin/tadot -i ${p} | dot -Tpdf -o `basename ${p} .plan`.pdf;
done

