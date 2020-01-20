<?php

if(!defined('BASE_URI')){
    require_once('../includes/config.inc.php');
    require_once('../includes/utils.inc.php');
    trigger_error("unauthorized direct module access");
    print_json_error("something went wrong, we are working on it...");
    exit();
}

use ParagonIE\Halite\KeyFactory;
use ParagonIE\HiddenString\HiddenString;

require_once('../includes/utils.inc.php');
require_once(DB);

$post_data = get_json_post_data(array('password','payload'));
$auth = new \Delight\Auth\Auth($conn);

$enc_key = KeyFactory::loadEncryptionKey('../modules/auth/encryption.key');

$plain_payload = unserialize(\ParagonIE\Halite\Symmetric\Crypto::decrypt(
    $post_data['payload'],
    $enc_key
));

$token = $plain_payload['token'];
$selector = $plain_payload['selector'];

if(!$token || !$selector){
	trigger_error('Could not retrieve encrypted payload');
    print_json_error("something went wrong, we are working on it...");
    exit();
}

try {
    $auth->resetPassword($selector, $token, $post_data['password']);
    print_json_message('PASSWORD UPDATE SUCCESS');
}
catch (\Delight\Auth\InvalidSelectorTokenPairException $e) {
    trigger_error('INVALID TOKEN');
    print_json_error('INVALID TOKEN');
    exit();
}
catch (\Delight\Auth\TokenExpiredException $e) {
    trigger_error('TOKEN EXPIRED');
    print_json_error('TOKEN EXPIRED');
    exit();    
}
catch (\Delight\Auth\ResetDisabledException $e) {
    trigger_error('PASSWORD RESET IS DISABLED');
    print_json_error('PASSWORD RESET IS DISABLED');
    exit();    
}
catch (\Delight\Auth\InvalidPasswordException $e) {
    trigger_error('INVALID PASSWORD');
    print_json_error('INVALID PASSWORD');
    exit();
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error('TOO MANY REQUESTS');
    print_json_error('TOO MANY REQUESTS');
    exit();
}

