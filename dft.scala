object dft
{
	def main(args: Array[String])
	{

		var pnum = 64

		var cvarr: Array[Double] = Array(1, 4, 8, 5, 3.5, 4.5, 7, -2, 3 , -5, -1, -7)

		var carr = cosPlot(pnum, cvarr) 
//		var carr = cosPlot(pnum, 1, 1) 

		for (i <- 0 until pnum) 
		{
			printf("%d ", i + 1)
			printf("%f ", carr(i))
			println(dft_rn(carr, i + 1))
		}
	}

	def sinPlot(pnum: Int, tt: Double, cv: Double): Array[Double] =
	{
		var arr: Array[Double] = Array.empty

		(0 until pnum).foreach (i =>	
			arr :+= cv * math.sin(2.0 * tt * math.Pi * i / pnum))

		arr
	}

	def cosPlot(pnum: Int, cvs: Array[Double]): Array[Double] = 
	{
		var arr: Array[Double] = Array.fill(pnum)(0)
		var sarr: Array[Double] = Array.empty

		for (i <- 0 until cvs.length)
		{
			sarr = cosPlot(pnum, (i + 1), cvs(i))

			(0 until sarr.length).foreach (n =>
				arr(n) += sarr(n)
			)
		}

		arr
	}

	def cosPlot(pnum: Int, tt: Double, cv: Double): Array[Double] =
	{
		var arr: Array[Double] = Array.empty

		(0 until pnum).foreach (i =>	
			arr :+= cv * math.cos(2.0 * tt * math.Pi * i / pnum))

		arr
	}


	def dft_rn(arr: Array[Double], k: Double): Double =
	{
		var cv = 2.0 * k * math.Pi / arr.length 
		var res = 0.0

		(0 until arr.length).foreach (i =>
			res += arr(i) * math.cos(i * cv)
		)

		res / arr.length
	}
}

