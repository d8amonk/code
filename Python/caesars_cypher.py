from string import maketrans, translate

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters_10on = letters[10:]
letters_upto10 = letters[:10]

shifted_letters = letters_10on + letters_upto10

encode_table = maketrans(letters, shifted_letters)
decode_table = maketrans(shifted_letters, letters)

message = "DAD'S A WANKER!"
encode_msg = message.translate(encode_table)
encode_msg 

secret_message = 'OD DE, LBEDEC!'
decoded_message = secret_message.translate(decode_table)
decoded_message
