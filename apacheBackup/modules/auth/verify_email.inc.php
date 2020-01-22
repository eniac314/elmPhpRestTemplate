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

$post_data = get_json_post_data(array('code','email'));
$auth = new \Delight\Auth\Auth($conn);
require_once('code_verification_utils.inc.php');


$result = get_email_verification_metadata($post_data['email'], $post_data['code']);

try {
    $auth->confirmEmail($result['selector'], $result['token']);
    $reply = ['codeVerificationPayload' => NULL];
    print_json_message($reply);
}
catch (\Delight\Auth\InvalidSelectorTokenPairException $e) {
    trigger_error('InvalidSelectorTokenPairException');
    print_json_error("INVALID SELECTOR TOKEN PAIR EXCEPTION");
    exit();
}
catch (\Delight\Auth\TokenExpiredException $e) {
    trigger_error('TokenExpiredException');
    print_json_error('TOKEN EXPIRED EXCEPTION');
    exit();
}
catch (\Delight\Auth\UserAlreadyExistsException $e) {
    trigger_error('UserAlreadyExistsException');
    print_json_error("USER ALREADY EXISTS EXCEPTION");
    exit();
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error('TooManyRequestsException');
    print_json_error("CODE VERIFICATION TOO MANY ATTEMPTS");
    exit();
}

