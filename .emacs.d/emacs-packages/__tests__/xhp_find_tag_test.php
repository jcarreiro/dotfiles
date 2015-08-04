<?hh
/*
;; testing helper. C-x e this
(progn
  (flet ((test (se)
               (save-excursion
                 (re-search-forward (car se))
                 (let ((as-expected (string-equal
                                     (xhp-mode-find-tag-at-point) (cdr se))))
                   (assert as-expected)
                   as-expected))))
    (let* ((search-expectations
            '(("some_global_function"               . "some_global_function")
              ("$some_typical_param"                . "$some_typical_param")
              ("$some_typical_param, "              . "$yet_another")
              ("$yet_another) {"                    . "$yet_another")
              ("some_global_function(\\$"           . "$some_typical_param")
              ("some_global_function(\\$"           . "$some_typical_param")
              ("class :m"                           . "m:some:clowny-class")
              ("extends :"                          . "m:base")
              ("self::"                             . "aStaticFunc")
              ("  se"                               . "self")
              (";\n.*:m:some"                       . "m:some:clowny-class")
              (":m:some:clowny-class::aStaticFunc"  . "aStaticFunc")
              ("m:some:clowny-child"                . "m:some:clowny-child")
              ("/x:demonstrating:closing"           . "x:demonstrating:closing")
              ("<m:base"                            . "m:base"))))
      (if (every #'identity (mapcar #'test search-expectations))
          (message "success!")
        (message "fail :(")))))
*/

function some_global_function($some_typical_param, $yet_another) {
}

class Foo {
  public static function bar() {
    return 1;
  }
}

abstract class :m:some:clowny-class extends :m:base {

  attribute
    :m:some:other-class;

  children (%m:some:clowny-child?);

  public function go() {
    self::aStaticFunc();
    :m:some:clowny-class::aStaticFunc();
    $an_unused_var_oh_well =
      <x:demonstrating:closing>
        {$this->go(/*stack overflow*/)}
      </x:demonstrating:closing>
    return <m:base />;
  }

  public static function aStaticFunc() {
    return 0;
  }
}
