package com.rpl;

public interface InterfaceAB extends InterfaceA, InterfaceB {
  int baz();
  int baz(long p);
  int baz(double p);
  int baz(int[] p);
  int baz(char[] p);
}
