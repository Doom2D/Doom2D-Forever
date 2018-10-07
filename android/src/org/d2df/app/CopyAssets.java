package org.d2df.app;

import android.content.Context;
import android.content.res.AssetManager;
import android.os.Environment;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.util.Log;

public class CopyAssets {

	private static boolean ExtStorageMounted() {
		return Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState());
	}

	private static boolean ExtStorageReadonly() {
		return Environment.MEDIA_MOUNTED_READ_ONLY.equals(Environment.getExternalStorageState());
	}

	private static void CopyFile(InputStream in, OutputStream out)
		throws IOException
	{
		byte[] buffer = new byte[1024];
		int read;
		while ((read = in.read(buffer)) != -1) {
			out.write(buffer, 0, read);
		}
	}

	public static void copyAssets(Context context, String prefix) {
		AssetManager assetManager = context.getAssets();
		String[] files = null;
		try {
			files = assetManager.list(prefix);
		} catch (IOException e) {
			Log.e("tag", "Failed to get asset file list.", e);
		}
		if (files != null) {
			for (String filename : files) {
				InputStream in = null;
				OutputStream out = null;
				try {
					if (ExtStorageMounted() && !ExtStorageReadonly()) {
						/* Get External Storage Path */
						File f = new File(context.getExternalFilesDir(null).getAbsolutePath(), prefix);
						if (!f.exists()) {
							f.mkdirs();
						}
						File outFile = new File(context.getExternalFilesDir(null).getAbsolutePath(), prefix + "/" + filename);
						if (!outFile.exists()) {
							in = assetManager.open(prefix + "/" + filename);
							out = new FileOutputStream(outFile);
							CopyFile(in, out);
						}
					} else {
						/* Get Internal Storage Path */
						File f = new File(context.getFilesDir().getAbsolutePath(), prefix);
						if (!f.exists()) {
							f.mkdirs();
						}
						File outFile = new File(context.getFilesDir().getAbsolutePath(), prefix + "/" + filename);
						if (!outFile.exists()) {
							in = assetManager.open(prefix + "/" + filename);
							out = new FileOutputStream(outFile);
							CopyFile(in, out);
						}
					}
				} catch(IOException e) {
					Log.e("tag", "Failed to copy asset file: " + filename, e);
				} finally {
					if (in != null) {
						try {
							in.close();
							in = null;
						} catch (IOException e) {}
					}
					if (out != null) {
						try {
							out.flush();
							out.close();
							out = null;
						} catch (IOException e) {}
					}
				}
			}
		}
	}

}
