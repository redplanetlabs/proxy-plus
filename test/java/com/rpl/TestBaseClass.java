package com.rpl;

public abstract class TestBaseClass {
  private String s;
  private int i;

  public TestBaseClass() {
    this.s = "s";
    this.i = 10;
  }

  public TestBaseClass(String s) {
    this.s = s;
    this.i = 11;
  }

  public TestBaseClass(String s, int i) {
    this.s = s;
    this.i = i;
  }

  public String getVal() {
    return s + " " + i;
  }

  protected abstract long foo(String s);
  protected abstract void foo2();

  public String doSomething(char c, long l, short s, double d) {
    throw new RuntimeException();
  }

  // Having many arguments is significant; Clojure only supports 4 args when
  // primative hints are present. See https://clojure.org/reference/java_interop
  public int hardSignature(byte b, short s, int i, long l, boolean bo, String st){
    return 1;
  }

  public Integer hardSignature(Byte b, Short s, Integer i, Long l, Boolean bo, String st){
    return 2;
  }
}
