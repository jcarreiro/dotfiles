<?php
/*
;; testing helper. C-x e this (note if you do this too fast before the
;; buffer is fully font-locked you may get an error too. page down to bottom to
;;  force fontification
 (progn
 (while (or
    (and (re-search-forward "''" nil t) (backward-char)) ;; all '' must be strs
    (re-search-forward "\\(not_\\)?font_locked" nil t))
  (let* (
         (not_str (match-string 1))
         (face (memq 'font-lock-string-face (text-properties-at (point))))
         )
    ;; if we're in a string that sould be font locked, the property better be
    ;; there likewise if we're not, it had better not
    (if (or
         (and not_str face)
         (and (not not_str) (not face)))
        (error "poorly formatted string"))))
  (message "success!!!"))
 */
function foo($a, $b) {
  $s0 = "&lt;;//font_locked";
  $s1 = '$a&lt;#font_locked?';
  $s2 = '$a&lt;/*font_locked?';
  $s3 = '<font_locked>?';
  $v = "''" ? $not_font_locked : 0;
  $w = '' ? $not_font_locked :  'font_locked?';
  $x = <x:frag />;
  $x0 = 'font_locked?';
  $y =
    <div>
      'not_font_locked'
      <div>foo</div>
    </div>;
    $y0 = 'font_locked?';
  $a = "doesn't".'look like a string? it should be font_locked.';
  $b = 'doesn\'t'.'look like a string? it should be font_locked.';
  $c =
    <div desc="wow">
      <div />
      <div desc="foo">
        not a string'{'does this look like a font_locked string?'}'and this not?
        'not_font_locked' 'not_font_locked here as well'
      </div>
      <div>
        {$bar}
      </div>
    </div>;
  // font lock can skip 500 characters at most by default
  // make > 500 character xhp block
  $d =
    <div desc="wow">
      <div desc="foo">
        not a string'{'does this look like a font_locked string?'}'and this not?
        'not_font_locked' 'not_font_locked here as well'
      </div>
      <div desc="bar">
        not a string'{'does this look like a font_locked string?'}'and this not?
        'not_font_locked' 'not_font_locked here as well'
      </div>
      <div desc="baz">
        not_font_locked'{'font_locked'}'not font locked
      <div>
      <div desc="cat">
        not_font_locked'{'font_locked'}'not font locked
      <div>
        {$bar}
      </div>
    </div>;
  $e = 'looks like a font_locked string';
}
