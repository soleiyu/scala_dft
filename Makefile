default:
#	rm sin.plt
	scalac wavgen.scala dft.scala
	scala dft
	scala dft > sin.plt
	gnuplot plot.txt
	eog res.png

clean:
	rm *.class
