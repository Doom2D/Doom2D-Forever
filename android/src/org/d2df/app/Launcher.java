package org.d2df.app;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

import android.content.*;

import android.view.*;
import android.widget.*;
import android.text.*;

import java.io.*;
import java.util.*;
import java.lang.*;

public class Launcher extends Activity {

  static final String preferences = "org.d2df.app.PREF";
  static final String prefArgs = "CommandLine";

  private EditText cmdline;

  private void saveCommandLine () {
    String s = cmdline.getText().toString();
    SharedPreferences sh = getSharedPreferences(preferences, MODE_PRIVATE);
    SharedPreferences.Editor ed = sh.edit();
    ed.putString(prefArgs, s);
    ed.apply();
  }

  private String loadCommandLine () {
    SharedPreferences sh = getSharedPreferences(preferences, MODE_PRIVATE);
    return sh.getString(prefArgs, "");
  }

  private class CmdLineWatcher implements TextWatcher {

    @Override
    public void beforeTextChanged (CharSequence s, int start, int count,int after) {
    }

    @Override
    public void onTextChanged (CharSequence s, int start, int before, int count) {
    }

    @Override
    public void afterTextChanged (Editable s) {
      saveCommandLine();
    }
  }

  private View.OnClickListener StartVavoomEvent = new View.OnClickListener () {

    public void onClick (View view) {
      Intent intent = new Intent(Launcher.this, Doom2DF.class);
      String s = Launcher.this.cmdline.getText().toString();
      intent.putExtra(prefArgs, s);
      Launcher.this.startActivity(intent);
    }
  };

  private void addImage (ViewGroup g, int id) {
    ImageView v = new ImageView(this);
    v.setImageResource(id);
    g.addView(v);
  }

  private void addText (ViewGroup g, String s) {
    TextView v = new TextView(this);
    v.setText(s);
    // v.setTextSize(16);
    g.addView(v);
  }

  private EditText addTextField (ViewGroup g, String s, TextWatcher w) {
    EditText v = new EditText(this);
    v.setText(s);
    v.addTextChangedListener(w);
    g.addView(v);
    return v;
  }

  private void addButton (ViewGroup g, String s, View.OnClickListener c) {
    Button v = new Button(this);
    v.setText(s);
    v.setOnClickListener(c);
    g.addView(v);
  }

  private void rebuild () {
    LinearLayout layout = new LinearLayout(this);
    layout.setOrientation(LinearLayout.VERTICAL);
//    addImage(layout, R.drawable.doom2df_logo);
    addText(layout, "Command line:");
    cmdline = addTextField(layout, loadCommandLine(), new CmdLineWatcher());
    addButton(layout, "Start", StartVavoomEvent);
    setContentView(layout);
  }

  @Override
  public void onCreate (Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    requestWindowFeature(Window.FEATURE_NO_TITLE);
    rebuild();
  }

}
