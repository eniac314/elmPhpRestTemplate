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

$auth = new \Delight\Auth\Auth($conn);

try {
    $auth->logOutEverywhere();
    print_json_message('LOGOUT SUCCESS');
}
catch (\Delight\Auth\NotLoggedInException $e) {
    trigger_error('NOT LOGGED IN');
    print_json_error('NOT LOGGED IN');
    exit();
}