import sys
import queue

def solve(Lin, Rin, Lout, Rout):
	seen_set = {(Lin, Rin)}
	q = queue.Queue()
	q.put((Lin,Rin,'i'))
	seen_states = 0	
	if Lin >= Lout and Rin <= Rout:
		return "EMPTY"
	
	while(not q.empty()):
		seen_states = seen_states + 1

		if seen_states > 1000000:
			break		
			
		(a, b, c) = q.get()
		a2, b2 = a//2, b//2
		a3, b3 = 3*a+1, 3*b+1	
	
		if (a2, b2) not in seen_set:
			seen_set.add((a2,b2))
			s = c + 'h'
			q.put((a2,b2,s))
			if a2 >= Lout and b2 <= Rout:
				return s[1:]
				
		if (a3, b3) not in seen_set and b3 < 1000000:
			seen_set.add((a3,b3))
			s = c + 't'
			q.put((a3,b3,s))	
			if a3 >= Lout and b3 <= Rout:
				return s[1:]

	return "IMPOSSIBLE"
				
with open(sys.argv[1],"r") as f:
	Q = int(next(f))
	#print(Q)	
	for line in f:	
		Lin, Rin, Lout, Rout = map(int, line.split())
		solution = solve(Lin, Rin, Lout, Rout)
		print(solution)
		#print(Lin, Rin, Lout, Rout)
