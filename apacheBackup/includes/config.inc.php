<?php # config.inc.php

# ****************** #
# *****SETTINGS***** #

// Errors are emailed here
$contact_email = 'florian.gillard@tutanota.com';

// Determine local or prod server
$host = substr($_SERVER['HTTP_HOST'],0,5);
if (in_array($host, array('local','127.0','192.1'))){
	$local = TRUE;
} else {
	$local = FALSE;
}

// Determine location of files and the URL of the site
// Allow for development on different servers
if ($local){

	// Define the constants:
	define('BASE_URI', '/var/www/html/');
	define('BASE_URL', 'http://localhost/public/');
	define('DB','/var/www/html/includes/localDB.inc.php');
	define('LOGFILE', '/var/www/html/errors.log');

} else {
	define('PROJECT_NAME', '');
	define('BASE_URI', '/kunden/homepages/11/d289158017/htdocs/MYPROJECT/');
	define('BASE_URL', '');
	define('DB',BASE_URI.PROJECT_NAME.'/includes/prodDB.inc.php');
	define('LOGFILE', BASE_URI.PROJECT_NAME.'/errors.log');
}

# *****SETTINGS***** #
# ****************** #


# ************************** #
# *****ERROR MANAGEMENT***** #

// display parsing errors
// ref: https://stackify.com/display-php-errors/
if($local){
	ini_set('display_errors', 1);
	ini_set('display_startup_errors', 1);
	error_reporting(E_ALL);
}

// create error handler 
function custom_error_handler($e_number, $e_message, $e_file, $e_line, $e_vars){
	global $local, $contact_email;

	$email = "
		<p>An error ($e_number) occurred on line 
		<strong>$e_line</strong> and in the <strong>file: $e_file.</strong> 
		<p> $e_message </p>";
		
	$email .= "<pre>" . print_r($e_vars, 1) . "</pre>";
	
	$headers = 'Content-type: text/html; charset=iso-8859-1' . "\r\n";

	$log =  "# ******** ".date("D M d, Y G:i", time())."*************************************************** #\r\n";
	$log .= "An error occurred in script '$e_file' on line $e_line: $e_message\r\n";
	$log .= print_r($e_vars, 1);
	$log .= print_r(debug_backtrace(), 1);
	$log .= "\r\n\r\n\r\n";

	if (!$local){
		error_log($email, 1, $contact_email, $headers);
	} 
	error_log($log, 3, LOGFILE);

}

set_error_handler('custom_error_handler');

# *****ERROR MANAGEMENT***** #
# ************************** #
