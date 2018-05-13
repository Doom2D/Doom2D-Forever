package org.d2df.app;

import android.content.Context;
import android.content.res.AssetManager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.util.Log;

public class CopyAssets {

  public static void copyAssets(Context context, String prefix) {
    AssetManager assetManager = context.getAssets();
    String[] files = null;
    try {
        files = assetManager.list(prefix);
    } catch (IOException e) {
        Log.e("tag", "Failed to get asset file list.", e);
    }
    if (files != null) for (String filename : files) {
        InputStream in = null;
        OutputStream out = null;
        try {
          File f = new File(context.getExternalFilesDir(null), prefix);
          if (!f.exists()) {
            f.mkdirs();
          }
          File outFile = new File(context.getExternalFilesDir(null), prefix + "/" + filename);
          if (!outFile.exists()) {
            in = assetManager.open(prefix + "/" + filename);
            out = new FileOutputStream(outFile);
            copyFile(in, out);
          }
        } catch(IOException e) {
            Log.e("tag", "Failed to copy asset file: " + filename, e);
        }     
        finally {
            if (in != null) {
                try {
                    in.close();
                    in = null;
                } catch (IOException e) {
                    
                }
            }
            if (out != null) {
                try {
                    out.flush();
                    out.close();
                    out = null;
                } catch (IOException e) {
                    
                }
            }
        }  
    }
  }
  
  public static void copyFile(InputStream in, OutputStream out) throws IOException {
    byte[] buffer = new byte[1024];
    int read;
    while((read = in.read(buffer)) != -1){
      out.write(buffer, 0, read);
    }
  }
  
}
