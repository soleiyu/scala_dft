default:
#	rm sin.plt
	scalac dft.scala
	scala dft
	scala dft > sin.plt
	gnuplot plot.txt
	eog res.png
