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

if(!isset($auth)){
	trigger_error("this module need an auth object");
	print_json_error("something went wrong, we are working on it...");
	exit();
}

function set_email_verification_metadada($code, $email, $selector, $token){
	global $conn;
	try{
		$stmt = $conn->prepare("INSERT INTO verification_codes_to_selectors_and_tokens (verification_code, selector, token, email, expires) VALUES (:code, :selector, :token, :email, NOW() + INTERVAL 5 MINUTE)");
		$stmt->execute([':code' => $code
	                   ,':email' => $email
	                   ,':selector' => $selector
	                   ,':token' => $token
	               	   ]);
	}
	catch(PDOException $e) {
        trigger_error("Database error: " . $e->getMessage());
		print_json_error("something went wrong, we are working on it...");
		exit();
    }
}

// this function returns the metadata needed to confirm a user email if the verification
// code exists and is correct. It first checks for expired code and delete corresponding
// rows. The function is throttled to 3 call per minute.
function get_email_verification_metadata($email, $code){
	global $conn;
	try {
    	// throttle the specified resource or feature to *3* requests per *60* seconds
    	$auth->throttle([ 'code-verification-request', $_SERVER['REMOTE_ADDR' ]], 3, 60);

    	try{
	    	//remove expired entries
	    	$conn->query("DELETE FROM verification_codes_to_selectors_and_tokens WHERE expires < NOW()");

	    	$stmt = $conn->prepare("SELECT * FROM verification_codes_to_selectors_and_tokens WHERE verification_code=:code");
			$stmt->execute([':code' => $code]); 
			$res = $stmt->fetch();
		}
    	catch(PDOException $e) {
        	trigger_error("Database error: " . $e->getMessage());
			print_json_error("something went wrong, we are working on it...");
			exit();
    	}

    	if (empty($res)){
    		trigger_error("Invalid or expired code");
			print_json_error("Invalid code");
			exit();	
    	}

    	if ($res['email'] !== $email){
    		trigger_error("Invalid code/mail assoctiation: $res");
			print_json_error("something went wrong, we are working on it...");
			exit();
    	}

    	return array('token' => $res['token'], 'selector' => $res['selector']);
	} // end of throttled bloc 
	catch (\Delight\Auth\TooManyRequestsException $e) {
    	trigger_error("too many code verification requests");
		print_json_error("too many code verification requests");
    	exit;
	}
}// end of get_email_verification_metadata function