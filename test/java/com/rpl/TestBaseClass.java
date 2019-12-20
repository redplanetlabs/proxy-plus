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

}
