<?php
// highlight the buffer and hit tab, see that it isn't modified
function foo() {
  if ($foo < 1 && $foo > 0) {
    //<xhp-in-comment>
    $a =
      <div>
        <div />
        <div>
          {$foo}
        </div>
        <div>
          {$bar}
        </div>
      </div>;
    $a->appendChild(
      <div>
        <a>
          <fbt desc="foo">Hello there!</fbt>
        </a>
      </div>
    );
    $a = array(
      AppRecPlatformType::CUSTOM_OPEN_GRAPH =>
      <fbt desc="platform type topnav filter, appcenter">All</fbt>,
      AppRecPlatformType::CUSTOM_OPEN_GRAPH =>
      <div>
        <ui:link
          href={self::getStoreURI($platform_filter, $this->categoryNs)}>
          Foo
        </ui:link>
      </div>
    );
  }
  $a = array(
    'foo' =>
    1
  );
  $a->appendChild(
    <foo>
      {$result['foo']}
      ({date('Y-m-d', strtotime($res['dateTaken']))})
    </foo>
  );
  switch ($this->platform) {
    case AppRecPlatformTypes::CUSTOM_OPEN_GRAPH:
      $top_left = $this->renderTimelineTitle();
    default:
      $top_left =
        <ui:link href="/apps/center/get-spotify">
          <ui:image src="images/appcenter/spotify-banner-homepage.png" />
        </ui:link>;
    }
  return
    <ui:box type="white">
      <ui:section-header label={$header} />
      {$list}
    </ui:box>;
}
function fillparagraph_tests() {
  $long_xhp =
    <div class="appsListHeader appsListRankingDescription" style="display:none border:none">
      Foo
    </div>;
  $long_self_closing_xhp =
    <fb:appcenter:friendsusing-list apprecresults={$list} platform-namespace={$platform_namespace} viewercontext={$vc} />;
  $foo = extra_long_function_call($with, $lots, $of, $parameters, $foo, $bar, $baz);
  $long_string_literal = "this should break at whitespace near the end of the line";
  if ($you || $have || $lots || ($expressions() && $in) || $an || $if || $statement) {
  }
  $long_string_literal_with_concats = 'this '.'$should'.' break '.$at.' the $at concat op';
  $long_mathematical_expressions = $this + $should * $break / $at + $the_next + $math + $op;
  if ($is_debug && $really_is_debug && (!$ctxt || !idx($ctxt,AppRecPreparable::FRIENDS_USING))) {
  }
  $appcenter_context = $this->getAttribute('appcentercontext')->getPlatformFilter();
  // not supported yet: call with only one arg, subfunction call
  // $related_apps_by_appid = yield wait_for($this->genRelatedApps($apprec_results));
  // <selector><ui:selector:option value="small">Small (180x115)</ui:selector:option></selector>
}
function heredoc_tests () {
  $request_script = <<<EOS
  FB.Monitor.disableLogging();
  FB.Bootstrap._requireFeatures(['Api'], function() {
    FB.XdHttpRequestServer.main({
      receiver: '/static/v0.4/xd_receiver.php',
      allowed_url_filter: '^/(restserver|session_state)\.php*'
    });
  });
EOS;
  // comments to make sure indentation works after heredoc
  $foo = <<<FOO
asdf
FOO;
  $foo = <<<'FOO'
$foo = <<<BAR
asdf
FOO;
  //< $foo = <<<FOO
  $not_in_heredoc = true;
  $foo = <<<FOO
unterminated heredoc, woohoo!
}

     not valid php down here, we're in unterminated heredoc!
