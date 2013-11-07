#ifndef _CURVE25519_H_
#define _CURVE25519_H_

int curve25519_dh_keypair(unsigned char *pk,unsigned char *sk);
int curve25519_dh(unsigned char *s,const unsigned char *pk,const unsigned char *sk);

#endif /* _CURVE25519_H_ */
