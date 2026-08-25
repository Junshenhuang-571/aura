/* aura_net.c — minimal blocking HTTP/1.1 client over TCP (WinSock2 / BSD). */
#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32")
#else
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>
#endif
#include <string.h>
#include <stdio.h>

static int net_initialized = 0;

static int net_init(void)
{
#ifdef _WIN32
    WSADATA wsa;
    if (!net_initialized) {
        if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0) return -1;
        net_initialized = 1;
    }
#endif
    return 0;
}

/* Perform POST http://host:port/path with JSON body; response written to
 * resp (NUL-terminated), truncated to resp_size-1.
 * Returns HTTP status code or negative on error. */
int aura_http_post(const char* host, int port, const char* path,
                   const char* body, char* resp, int resp_size)
{
    int sock = -1;
    struct addrinfo hints, *res = NULL, *rp;
    char portstr[16];
    char header[512];
    char* request;
    int req_len, total, sent, n;
    int status = -1;

    if (net_init() != 0) return -100;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    snprintf(portstr, sizeof(portstr), "%d", port);
    if (getaddrinfo(host, portstr, &hints, &res) != 0) return -101;

    for (rp = res; rp; rp = rp->ai_next) {
        sock = (int)socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock < 0) continue;
        if (connect(sock, rp->ai_addr, (int)rp->ai_addrlen) == 0) break;
#ifdef _WIN32
        closesocket(sock);
#else
        close(sock);
#endif
        sock = -1;
    }
    freeaddrinfo(res);
    if (sock < 0) return -102;

    snprintf(header, sizeof(header),
             "POST %s HTTP/1.1\r\nHost: %s:%d\r\nContent-Type: application/json\r\n"
             "Content-Length: %d\r\nConnection: close\r\n\r\n",
             path, host, port, (int)strlen(body));
    req_len = (int)(strlen(header) + strlen(body));
    request = (char*)malloc(req_len + 1);
    if (!request) { status = -103; goto out; }
    memcpy(request, header, strlen(header));
    memcpy(request + strlen(header), body, strlen(body));

    sent = 0;
    while (sent < req_len) {
#ifdef _WIN32
        n = send(sock, request + sent, req_len - sent, 0);
#else
        n = (int)send(sock, request + sent, req_len - sent, 0);
#endif
        if (n <= 0) { status = -104; free(request); goto out; }
        sent += n;
    }
    free(request);

    /* read full response */
    total = 0;
    for (;;) {
#ifdef _WIN32
        n = recv(sock, resp + total, resp_size - 1 - total, 0);
#else
        n = (int)recv(sock, resp + total, resp_size - 1 - total, 0);
#endif
        if (n <= 0) break;
        total += n;
        if (total >= resp_size - 1) break;
    }
    resp[total] = '\0';

    /* parse "HTTP/1.x NNN" */
    {
        const char* p = strstr(resp, "HTTP/");
        if (p) {
            p = strchr(p, ' ');
            if (p) status = atoi(p + 1);
        }
    }

out:
#ifdef _WIN32
    closesocket(sock);
#else
    close(sock);
#endif
    return status;
}
