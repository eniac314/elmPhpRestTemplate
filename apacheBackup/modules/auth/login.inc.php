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

$post_data = get_json_post_data(array('password','username'));
$auth = new \Delight\Auth\Auth($conn);

try {
    $auth->loginWithUsername($post_data['username'], $post_data['password']);
    
    $username = $auth->getUsername();
    $email = $auth->getEmail();	
    $roles = $auth->getRoles();

    $reply = ['username' => $username
             ,'email' => $email
             ,'roles' => $roles
             ];
    
    print_json_message($reply);
    exit();
}
catch (\Delight\Auth\InvalidEmailException $e) {
    trigger_error("INVALID EMAIL ADDRESS");
    print_json_error("INVALID EMAIL ADDRESS");
    exit;
}
catch (\Delight\Auth\UnknownUsernameException $e) {
    trigger_error("UNKNOWN USERNAME");
    print_json_error("UNKNOWN USERNAME");
    exit;
}
catch (\Delight\Auth\InvalidPasswordException $e) {
    trigger_error("WRONG CREDENTIALS");
    print_json_error("WRONG CREDENTIALS");
    exit;
}
catch (\Delight\Auth\EmailNotVerifiedException $e) {
    trigger_error("NEED EMAIL VERIFICATION");
    print_json_error("NEED EMAIL VERIFICATION");
    exit;
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error("TOO MANY REQUESTS");
    print_json_error("TOO MANY REQUESTS");
    exit;
}