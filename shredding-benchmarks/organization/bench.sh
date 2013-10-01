query=$1
echo $query
for scale in 16 32 64 128 256; do
	rm -f res;
	for i in `seq 1 20`; do
		psql organisation${scale} < ${query}.sql | grep runtime | awk '{ print $3; }' >> res;
	done;
	cat res | python -c 'from sys import stdin; nums = [float(i) for i in stdin.read().split()]; print(sum(nums)/len(nums))'
done

