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


try{
    $conn->query("DELETE FROM verification_codes_to_selectors_and_tokens WHERE expires < NOW()");
    $stmt = $conn->prepare("SELECT selector, token FROM verification_codes_to_selectors_and_tokens WHERE verification_code=:code AND email=:email");
    $stmt->execute([':code' => $post_data['code']
                   ,':email' => $post_data['email']
                   ]);
    $result = $stmt->fetch();

    if(!$result){
        trigger_error("No corresponding code");
        print_json_error("CODE VERIFICATION FAILURE");
        exit();
    }

    $stmt = $conn->prepare("DELETE FROM verification_codes_to_selectors_and_tokens WHERE verification_code=:code AND email=:email");
    $stmt->execute([':code' => $post_data['code']
                   ,':email' => $post_data['email']
                   ]);

}
catch(PDOException $e) {
    trigger_error("Database error: " . $e->getMessage());
    print_json_error("something went wrong, we are working on it...");
    exit();
}


try {
    $auth->confirmEmail($result['selector'], $result['token']);
    $reply = ['codeVerificationPayload' => NULL];
    print_json_message($reply);
}
catch (\Delight\Auth\InvalidSelectorTokenPairException $e) {
    trigger_error('InvalidSelectorTokenPairException');
    print_json_error("CODE VERIFICATION FAILURE");
    exit();
}
catch (\Delight\Auth\TokenExpiredException $e) {
    trigger_error('TokenExpiredException');
    print_json_error("CODE VERIFICATION FAILURE");
    exit();
}
catch (\Delight\Auth\UserAlreadyExistsException $e) {
    trigger_error('UserAlreadyExistsException');
    print_json_error("CODE VERIFICATION FAILURE");
    exit();
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error('TooManyRequestsException');
    print_json_error("CODE VERIFICATION TOO MANY ATTEMPTS");
    exit();
}

