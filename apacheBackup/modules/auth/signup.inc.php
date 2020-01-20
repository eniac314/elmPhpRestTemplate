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

$post_data = get_json_post_data(array('email','password','username'));
$auth = new \Delight\Auth\Auth($conn);

require_once('code_verification_utils.inc.php');


try {
    $userId = $auth->register($post_data['email'], $post_data['password'], $post_data['username'], function ($selector, $token) use ($post_data){
        $code = random_int(100000,999999);
        set_email_verification_metadada($code, $post_data['email'], $selector, $token);
        $send = 
            function(){
                mail($post_data['email'],'Verification email',$code,'Content-type: text/html; charset=iso-8859-1' . "\r\n");
            };
        // register_shutdown_function($send);
    });

    print_json_message("SIGNUP SUCCESSFUL");
    exit();
}
catch (\Delight\Auth\InvalidEmailException $e) {
    trigger_error("INVALID EMAIL ADDRESS");
    print_json_error("INVALID EMAIL ADDRESS");
    exit;
}
catch (\Delight\Auth\InvalidPasswordException $e) {
    trigger_error("INVALID PASSWORD");
    print_json_error("INVALID PASSWORD");
    exit;
}
catch (\Delight\Auth\UserAlreadyExistsException $e) {
    trigger_error("USER ALREADY EXISTS");
    print_json_error("USER ALREADY EXISTS");
    exit;
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error("TOO MANY REQUESTS");
    print_json_error("TOO MANY REQUESTS");
    exit;
}
