<?php #api.php
/* this is the endpoint of the REST api
 * this page include the configuration file
 * and the request specific modules 
 */

// Require composer auto loader
require_once('../vendor/autoload.php');

// Require the configuration file between anything else is run 
require_once('../includes/config.inc.php');

if (isset($_GET['module'])){
	$module = $_GET['module'];
} else {
	trigger_error("No module has been requested");
	exit();
}

switch ($module) {
	case 'dummy':
		$module_filename = 'dummy.inc.php';
		break;
	
	default:
		trigger_error("No corresponding module name");
		exit();
		break;
}

if (!file_exists('../modules/'.$module_filename)){
	trigger_error("No corresponding module file");
	exit();
}
include('../modules/'.$module_filename);

?>