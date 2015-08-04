<?hh

/**
 * Helper class for testing hh-complete.
 *
 * NOTE: this must be copied to
 * a www directory tree to work and hh_server must be running,
 * e.g. /home/engshare/tools/hh_server ~/www
 */
class X {
  private bool $baz;
  public function bar(): string {
    // TEST HERE
    $this->fooba;
  }
  public function foo(string $a): int {return 0;}
  public function foobar() {}
  public function fooBarAVeryLongFunctionName() {}
}
