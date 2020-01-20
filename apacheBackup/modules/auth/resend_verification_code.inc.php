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

$post_data = get_json_post_data(array('email'));
$auth = new \Delight\Auth\Auth($conn);
require_once('./code_verification_utils.inc.php');


try{
    $conn->query("DELETE FROM verification_codes_to_selectors_and_tokens WHERE expires < NOW()");

    $stmt = $conn->prepare("DELETE FROM verification_codes_to_selectors_and_tokens WHERE email=:email");
    $stmt->execute([':email' => $post_data['email']]);

}
catch(PDOException $e) {
    trigger_error("Database error: " . $e->getMessage());
    print_json_error("something went wrong, we are working on it...");
    exit();
}

try {
    $auth->resendConfirmationForEmail($post_data['email'], function ($selector, $token) use ($post_data){
        $code = random_int(100000,999999);
        set_email_verification_metadada($code, $post_data['email'], $selector, $token);
        $send = 
        	function(){
        		mail($post_data['email'],'Verification email',$code,'Content-type: text/html; charset=iso-8859-1' . "\r\n");
        	};
        // register_shutdown_function($send);
        
        
	});
	print_json_message("RESEND CODE VERIFICATION SUCCESS");
	exit();

}
catch (\Delight\Auth\ConfirmationRequestNotFound $e) {
    trigger_error("RESEND CODE NO PREVIOUS ATTEMPT");
    print_json_error("RESEND CODE NO PREVIOUS ATTEMPT");
    exit();
}
catch (\Delight\Auth\TooManyRequestsException $e) {
    trigger_error("RESEND CODE TOO MANY ATTEMPS");
    print_json_error("RESEND CODE TOO MANY ATTEMPS");
    exit();
}