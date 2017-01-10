pro caca
	n1 = 120
	n2 = 72
	a = fltarr(n1,n2)
	openr,2,'fort.18'
	readf,2,a
	close,2

	b = fltarr(n2)
	openr,2,'fort.19'
	readf,2,b
	close,2

	print, max(a), min(a)
	print, max(b), min(b)

stop
end