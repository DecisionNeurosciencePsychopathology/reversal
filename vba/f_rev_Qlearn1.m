function  [fx] = f_rev_Qlearn1(x,theta,u,inF)

% function  [fx,dfdx,dfdP] = f_trust_Qlearn_counter(x,theta,u,inF)
% evolution function of q-values of a RL agent (1-armed bandit problem)
% [fx,dfdx,dfdP] = f_Qlearn1(x,P,u,in)
% Here, there is only one q-value to evolve (two action values are
% reciprocal)

% IN:
%   - x_t : q-values (1x1)
%   - P : (inverse-sigmoid) learning-rate
%   - u : u(1)=previous action (1 or 0), u(2)=feedback
%   - in : [useless]
% OUT:
%   - fx: evolved q-values (1x1)
%   - dfdx/dfdP: gradient of the q-values evolution function, wrt q-values
%   and evolution parameter, respectively.F 

r = u(2); % make sure rewards are coded 0 = no reward, 1 = reward
 % theta(1) -- basic learning rate


alpha = 1./(1+exp(-theta(1))); % learning rate is bounded between 0 and 1.

if u(1)==1
pe = r-x(1); % prediction error
elseif u(1)==2
pe = r-(1-x(1)); % prediction error
end    
fx = zeros(length(x),1);

fx(1) = x(1) + alpha*pe;

%tracking PEs for RT analyses
fx(2) = pe;

%% one hidden state (value)
% dfdx = zeros(size(x,1),1);
% dfdx(1) = 1-alpha;
% dfdP = alpha*(1-alpha)*pe;

%% two hidden states (value + pe)
% gradients' derivation
% if u(1)==1
%     dfdx = [1-alpha, 0;
%             0, 1];
%     dfdP = [alpha*(1-alpha)*pe(1),0];
% else
%     dfdx = [1, 0;
%             0, 1-alpha];
%     dfdP = [0,alpha*(1-alpha)*pe(2)];
% end
