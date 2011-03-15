<?php
define('_SAPE_USER', 'ec412122841ba6bb52b8920985b75eda');
require_once(_SAPE_USER.'/sape.php');
/* $o['force_show_code'] = true; */
$o['host'] = 'rigidus.ru';
$sape = new SAPE_client($o);
echo($sape->return_links());
/* echo 'env-request-uri: '.getenv('REQUEST_URI'), "<br />\n"; */
?>