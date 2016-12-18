function [U, R, Q, X] = analyze_mymodel(result_path, nrServers, nrThreads, writeProp, arrivalRate, acceptorServiceTime, readServiceTime, writeServiceTime, writeDelayTime)

pkg load queueing;

% Read proportion
readProp = 1 - writeProp

% nK: number of service centers
nk = 1 + 3 * nrServers % One acceptor and three service centers per server.

% Center 1: Acceptor center
% Center 2: Read center for server 1: M/M/nrThreads
% Center 3: Write center for server 1: M/M/1
% Center 4: Write delay center for server 1: M/M/inf
% Center 2 + 3 * i    :  read center for server i
% Center 2 + 3 * i + 1:  write center for server i
% Center 2 + 3 * i + 2:  write delay center for server i

% Lamba: The vector of overal arrival rates
lambda = zeros(nk, 1);
lambda(1,1) = arrivalRate;

% P(i, j): The probability that a request which completed service at center i is routed to center j
P = zeros(nk, nk);

acceptorCenter = 1;

for i = 0:(nrServers -1)
  readCenter = 2 + 3 * i;
  writeCenter = readCenter + 1;
  delayCenter = writeCenter + 1;

  P(acceptorCenter, readCenter) = readProp * (1 / nrServers);
  P(acceptorCenter, writeCenter) = writeProp * (1 / nrServers);

  P(writeCenter, delayCenter) = 1;


endfor

% V[i]: average number of visits to center i
V = qnosvisits(P, lambda);


S = zeros(nk, 1);
m = zeros(nk, 1);

% S[i]: Average service time at center i
S(acceptorCenter) = acceptorServiceTime;
m(acceptorCenter) = 1;

for i = 0:(nrServers -1)
  readCenter = 2 + 3 * i;
  writeCenter = readCenter + 1;
  delayCenter = writeCenter + 1;

  S(readCenter) = readServiceTime;
  S(writeCenter) = writeServiceTime;
  S(delayCenter) = writeDelayTime;

  m(readCenter) = nrThreads;
  m(writeCenter) = 1;
  m(delayCenter) = 0; % Infinity
endfor

% Lamba: The overal arrival rate
lambda = arrivalRate;

% ---- Results ----
[U, R, Q, X] = qnos(lambda, S, V, m)


save(result_path, "-v7", "S", "V", "m", "U", "R", "Q", "X");

endfunction
