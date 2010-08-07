
package le;

import java.awt.Color;
import java.awt.image.BufferedImage;

import clojure.lang.PersistentVector;
import clojure.lang.PersistentHashSet;

public class Optimized {
	public static PersistentVector vector () {
		PersistentVector v = PersistentVector.EMPTY;
		v = v.cons(new Double(3.0));
		v = v.cons(new Double(3.0));
		return v;
	}
	public static int ugh () {
		return 5;
	}
	static public PersistentVector getRGB(BufferedImage i, int x, int y) {
		int pixel = i.getRGB(x, y);

		PersistentVector v = PersistentVector.EMPTY;
		//v = v.cons((new Double((pixel >> 24) & 0xFF)) / 255.0); //A
		//v = v.cons((new Double((pixel >> 16) & 0xFF)) / 255.0); //r
		//v = v.cons((new Double((pixel >> 8) & 0xFF)) / 255.0); //g
		//v = v.cons((new Double(pixel & 0xFF)) / 255.0); //b
		return v;
	}
	
	static public double[] fastGetRGB(BufferedImage i, int x, int y) {
			int pixel = i.getRGB(x, y);
			double[] cols = new double[4];
			cols[0] = (double)((pixel >> 24)  & 0xFF) / 255.0; //a
			cols[1] = (double)((pixel >> 16)  & 0xFF) / 255.0; //r
			cols[2] = (double)((pixel >>  8)  & 0xFF) / 255.0; //g
			cols[3] = (double)((pixel & 0xFF) & 0xFF) / 255.0; //b
			return cols;
	}
		
	static public BufferedImage setHSV(BufferedImage i, int x, int y, double h, double s, double v)
	{
		int col = Color.HSBtoRGB((float)h, (float)s, (float)v);
		i.setRGB(x, y, col);
		return i;
	}
	static public int[][][] convertToCheckable(BufferedImage im) {
		int[][][] matrix = new int[im.getWidth()][im.getHeight()][2];
		for(int i = 0; i < im.getWidth(); i++) {
			for(int j = 0; j < im.getHeight(); j++) {
				int pixel = im.getRGB(i, j);
				matrix[i][j][0] = (pixel >> 24)  & 0xFF; //alpha
				matrix[i][j][1] = pixel  & 0xFFFFFF; //all except alpha
			}
		}
		return matrix;
	}
	
	//caution, this is ultimate speedy.	Returns number of nonmatching pixels.
	//returns 10000 if it gives up.
	public static int fastImgFilter (int[][][] filtmat, int[][][] imgmat, int x, int y)
	{
		int errorthresh = 3; //allow this many non-matching pixels before giving up.
		int alphathresh = 250; //ignore any alpha less than this.
		
		int fwidth = Math.min(filtmat.length, (imgmat.length - x));
		int fheight = Math.min(filtmat[0].length, (imgmat[0].length - y));
		int total_errors = 0;
		
		for(int i = 0; i < fwidth; i++)
			for(int j = 0; j < fheight; j++) {
				//double[] pix1 = fastGetRGB(filt, i, j);
				//double[] pix2 = fastGetRGB(img, i+x, j+y);
				//if alpha is nonzero
				if(filtmat[i][j][0] >= alphathresh ) {
					if(filtmat[i][j][1] != imgmat[i+x][j+y][1]) //check "payload" color data
						total_errors += 1;
						//here's the optimization. Return some arbitrary high number.
						if(total_errors > errorthresh)
							return 10000;
				}
			}
		return total_errors;
	}
	
	//caution, this is fudged for speed, if an error goes above epsilon, it
	//returns 1.0, since we're only concerned w/ matches of <e error.	
	public static double imgFilter (BufferedImage filt, BufferedImage img, int x, int y)
	{
		double epsilon = 0.0001;
		
		int fwidth = Math.min(filt.getWidth(), (img.getWidth() - x));
		int fheight = Math.min(filt.getHeight(), (img.getHeight() - y));
		double total_error = 0.0;
		
		for(int i = 0; i < fwidth; i++)
			for(int j = 0; j < fheight; j++) {
				double[] pix1 = fastGetRGB(filt, i, j);
				double[] pix2 = fastGetRGB(img, i+x, j+y);
				//if alpha is nonzero
				if(pix1[0] > epsilon) {
					for(int c = 1; c <= 3; c++) {
						double error = (pix1[c] - pix2[c]);
						total_error += error * error;
						//here's the optimization.
						if(total_error > epsilon)
							return 1.0;
					}
				}
			}
		return total_error / ((double)fwidth * (double)fheight * 3.0);
	}
	
	
	//returns a vector of [x, y, error] triplets that are lower than a relevant
	//error threshold.	Since imgFilter is early-out, we end up only getting 
	//perfect matches.
	public static PersistentHashSet filterMap (BufferedImage filt, BufferedImage img)
	{
		int threshold = 3; //also set in fastImgFilter for some reason. FIXME.
		
		PersistentHashSet ret = PersistentHashSet.EMPTY;
		int width = img.getWidth() - filt.getWidth() + 1;
		int height = img.getHeight() - filt.getHeight() + 1;
		
		int[][][] filtmat = convertToCheckable(filt);
		int[][][] imgmat = convertToCheckable(img);
		
		for(int i = 0; i < width; i++)
			for(int j = 0; j < height; j++) {
				int error = fastImgFilter(filtmat, imgmat, i, j);
				if(error <= threshold) {
					PersistentVector pv = PersistentVector.EMPTY;
					pv = pv.cons(new Integer(i));
					pv = pv.cons(new Integer(j));
					// pv = pv.cons(error); zero anyway
					ret = (PersistentHashSet) ret.cons(pv);
				}
			}
		return ret;
	}
}
