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
		$module_path = 'dummy.inc.php';
		break;

	case 'signup':
		$module_path = 'auth/signup.inc.php';
		break;
	
	case 'verifyEmail':
		$module_path = 'auth/verify_email.inc.php';
		break;

	case 'newCode':
		$module_path = 'auth/resend_verification_code.inc.php';
		break;

	case 'initiatePasswordReset':
		$module_path ='auth/initiate_password_reset.inc.php';
		break;

	case 'verifyEmailForPasswordReset':
		$module_path = 'auth/verify_email_for_password_reset.inc.php';
		break;

	case 'updatePassword':
		$module_path ='auth/update_password_after_reset.inc.php';
		break;

	case 'login':
		$module_path = 'auth/login.inc.php';
		break;

	case 'logout':
		$module_path = 'auth/logout.inc.php';
		break;

	case 'refresh':
		$module_path = 'auth/refresh.inc.php';
		break;

	
	default:
		trigger_error("No corresponding route");
		echo "something went wrong, we are working on it...";
		exit();
		break;
}

if (!file_exists('../modules/'.$module_path)){
	trigger_error("No corresponding module file");
	echo "something went wrong, we are working on it...";
	exit();
}
include('../modules/'.$module_path);

?>