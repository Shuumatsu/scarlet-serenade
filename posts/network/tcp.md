---
title: Network | TCP
---

## Sender

```rust
pub enum State {
    Error, // sender.stream_in().error()
    Closed, // sender.next_seqno_absolute() == 0
    SynSent, // 
    SynAcked, // 
    FinSent,
    FinAcked,
}
```

## Receiver

```rust
pub enum State {
    Error, // receiver.stream_out().error()
    Listen, // !receiver.ackno().has_value()
    FinRecv, // receiver.stream_out().input_ended()
    SynRecv, 
}
```

## Connection

```c++
string TCPState::state_summary(const TCPSender &sender) {
    if (sender.stream_in().error()) {
        return TCPSenderStateSummary::ERROR;
    } else if (sender.next_seqno_absolute() == 0) {
        return TCPSenderStateSummary::CLOSED;
    } else if (sender.next_seqno_absolute() == sender.bytes_in_flight()) {
        return TCPSenderStateSummary::SYN_SENT;
    } else if (not sender.stream_in().eof()) {
        return TCPSenderStateSummary::SYN_ACKED;
    } else if (sender.next_seqno_absolute() < sender.stream_in().bytes_written() + 2) {
        return TCPSenderStateSummary::SYN_ACKED;
    } else if (sender.bytes_in_flight()) {
        return TCPSenderStateSummary::FIN_SENT;
    } else {
        return TCPSenderStateSummary::FIN_ACKED;
    }
}

enum class TCPState {
    LISTEN = 0,   //!< Listening for a peer to connect
    SYN_RCVD,     //!< Got the peer's SYN
    SYN_SENT,     //!< Sent a SYN to initiate a connection
    ESTABLISHED,  //!< Three-way handshake complete
    CLOSE_WAIT,   //!< Remote side has sent a FIN, connection is half-open
    LAST_ACK,     //!< Local side sent a FIN from CLOSE_WAIT, waiting for ACK
    FIN_WAIT_1,   //!< Sent a FIN to the remote side, not yet ACK'd
    FIN_WAIT_2,   //!< Received an ACK for previously-sent FIN
    CLOSING,      //!< Received a FIN just after we sent one
    TIME_WAIT,    //!< Both sides have sent FIN and ACK'd, waiting for 2 MSL
    CLOSED,       //!< A connection that has terminated normally
    RESET,        //!< A connection that terminated abnormally
};
```


## Connection Establishment Protocol

**Random ISN**: The purpose in these sequence numbers is to prevent packets that get delayed in the network from being delivered later and then misinterpreted as part of an existing connection.

1. The client sends a **SYN** segment with its own initial sequence number.
2. The server responds a **SYN** segment with its own initial sequence number while also acknowledges the client's **SYN** by **ACKing** the client's ISN plus one.
3. The client acknowledge this **SYN** from the server by **ACKing** the server's ISN plus one.



The side that sends the first SYN is said to perform an active open. The other side, which receives this SYN and sends the next SYN, performs a passive open.

## Connection Termination Protocol


A TCP connection is full-duplex, each direction must be shut down independently.

The rule is that either end can send a FIN when it is done sending data.

When a TCP receives a FIN, it must notify the application that the other end has terminated that direction of data flow. (The sending of a FIN is normally the result of the application issuing a close.

The receipt of a FIN only means there will be no more data flowing in that direction.


We say that the end that first issues the close (e.g., sends the first FIN) performs the **active close** and the other end (that receives this FIN) performs the **passive close**. 