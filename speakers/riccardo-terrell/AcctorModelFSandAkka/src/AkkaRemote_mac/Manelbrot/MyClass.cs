using System;
using System.Drawing;

namespace MandelbrotSet
{
	public static class Mandelbrot
	{
		public static Bitmap Set(int xp, int yp, int w, int h, int width, int height, double maxr, double minr, double maxi, double mini)
		{
			Bitmap img = new Bitmap(w, h);
			double zx = 0;
			double zy = 0;
			double cx = 0;
			double cy = 0;
			double xjump = ((maxr - minr) / Convert.ToDouble(width));
			double yjump = ((maxi - mini) / Convert.ToDouble(height));
			double tempzx = 0;
			int loopmax = 1000;
			int loopgo = 0;
			for (int x = xp; x < xp + w; x++)
			{
				cx = (xjump * x) - Math.Abs(minr);
				for (int y = yp; y < yp + h; y++)
				{
					zx = 0;
					zy = 0;
					cy = (yjump * y) - Math.Abs(mini);
					loopgo = 0;
					while (zx * zx + zy * zy <= 4 && loopgo < loopmax)
					{
						loopgo++;
						tempzx = zx;
						zx = (zx * zx) - (zy * zy) + cx;
						zy = (2 * tempzx * zy) + cy;
					}
					if (loopgo != loopmax)
						img.SetPixel(x - xp, y - yp, Color.FromArgb(loopgo % 32 * 7, loopgo % 128 * 2, loopgo % 16 * 14));
					else
						img.SetPixel(x - xp, y - yp, Color.Black);
				}
			}
			return img;
		}
	}
}
