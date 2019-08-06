object wavgen
{
	def ramPlot(pnum: Int, tt: Double, cv: Double): Array[Double] = 
	{
		var arr: Array[Double] = Array.empty

		(0 until pnum).foreach (i =>
			arr :+= cv * (2.0 * (i % (pnum / tt)) / pnum - 1))

		arr
	}

	def cramPlot(pnum: Int, tt: Double, cv: Double): Array[Double] = 
	{
		var arr: Array[Double] = Array.empty

		(0 until pnum).foreach (i =>
			arr :+= cv * (-2.0 * (i % (pnum / tt)) / (pnum / tt) + 1))

		arr
	}

	def hsinPlot(pnum: Int, cvs: Array[Double]): Array[Double] = 
	{
		var arr: Array[Double] = Array.fill(pnum)(0)
		var sarr: Array[Double] = Array.empty

		for (i <- 0 until cvs.length)
		{
			sarr = sinPlot(pnum, (i + 1.0) / 2.0, cvs(i))

			(0 until sarr.length).foreach (n =>
				arr(n) += sarr(n)
			)
		}

		arr
	}

	def sinPlot(pnum: Int, cvs: Array[Double]): Array[Double] = 
	{
		var arr: Array[Double] = Array.fill(pnum)(0)
		var sarr: Array[Double] = Array.empty

		for (i <- 0 until cvs.length)
		{
			sarr = sinPlot(pnum, (i + 1), cvs(i))

			(0 until sarr.length).foreach (n =>
				arr(n) += sarr(n)
			)
		}

		arr
	}

	def sinPlot(pnum: Int, tt: Double, cv: Double): Array[Double] =
	{
		var arr: Array[Double] = Array.empty

		(0 until pnum).foreach (i =>	
			arr :+= cv * math.sin(2.0 * tt * math.Pi * i / pnum))

		arr
	}

	def hcosPlot(pnum: Int, cvs: Array[Double]): Array[Double] = 
	{
		var arr: Array[Double] = Array.fill(pnum)(0)
		var sarr: Array[Double] = Array.empty

		for (i <- 0 until cvs.length)
		{
			sarr = cosPlot(pnum, (i + 1.0) / 2.0, cvs(i))

			(0 until sarr.length).foreach (n =>
				arr(n) += sarr(n)
			)
		}

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
}

