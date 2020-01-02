<?php

if(!defined('BASE_URI')){
	require_once('../includes/config.inc.php');
	require_once('../includes/utils.inc.php');
	trigger_error("unauthorized direct module access");
	print_json_error("something went wrong, we are working on it...");
	exit();
}

require_once('../includes/utils.inc.php');
require_once(DB);

print_json_message("this is a test");



