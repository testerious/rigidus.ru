<?php
// http://www.php.ru/manual/features.commandline.html
// http://help.sape.ru/sape/faq/270
/* echo 'php-current-dir: '.getcwd(), "<br />\n"; */
/* echo 'document-root: '.$_SERVER['DOCUMENT_ROOT'], "<br />\n"; */
/* echo 'env-request-uri: '.getenv('REQUEST_URI'), "<br />\n"; */
/* echo 'stdin: '.file_get_contents('php://stdin') , "<br />\n"; */

// Sape
define('_SAPE_USER', 'ec412122841ba6bb52b8920985b75eda');
require_once(_SAPE_USER.'/sape.php');
/* $o['force_show_code'] = true; */
$o['host'] = 'rigidus.ru';
/* $o['debug'] = true; */
$sape_context = new SAPE_context($o);
echo($sape_context->replace_in_text_segment(file_get_contents('php://stdin')));
?>