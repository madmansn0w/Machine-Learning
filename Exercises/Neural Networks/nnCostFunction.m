function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%

a1 = [ones(m, 1) X];
z2 = a1 * Theta1';
a2 = [ones(size(z2, 1), 1) sigmoid(z2)];
z3 = a2 * Theta2';
a3 = sigmoid(z3);
hx = a3;

% refactor y into a binary output vector, Y, using y(i) == k output value;
% e.g: y = [1, 2, 3, 4, 5] == 3 => y = [0, 0, 1, 0, 0]
% repmat is good for this:
%  -- repmat (A, M, N)
% Form a block matrix of size M by N, with a copy of matrix A as each element.
Y = repmat([1 : num_labels], m, 1) == repmat(y, 1, num_labels);

% cost function:
J = -1/m * sum( sum( Y .* log(hx) + (1 - Y) .* log(1 - hx) ), 2);

% remove bias from theta
theta1_bias_removed = Theta1(:, 2:end); # all rows, sans first element of column (bias);
theta2_bias_removed = Theta2(:, 2:end);
theta1_reg = Theta1; theta1_reg(:, 1) = 0;
theta2_reg = Theta2; theta2_reg(:, 1) = 0;

% penalty summation:
%  -- sum (X, DIM)
% ommitted DIM or DIM = 1 corresponds to summing the columns in sum(...)
% sum(sum(x , 2)) => sum matrix by row and then sum the resultant column vector
% therefore we need DIM = 2
p = sum(sum(theta1_bias_removed .^ 2), 2) + sum(sum(theta2_bias_removed .^ 2), 2); 

% regularization terms
regularization_terms = lambda/(2*m)*p;

% add summation error terms to cost function
J = J + regularization_terms;

% Back propogation

% error deltas
deltaL = a3 - Y;
delta2 = (deltaL * Theta2(:,2:end)) .* sigmoidGradient(z2);

% gradients in vector forms, cause screw for loops
Delta1 = delta2' * a1;
Delta2 = deltaL' * a2;

% regularization for gradient

p1 = (lambda/m) * theta1_reg;
p2 = (lambda/m) * theta2_reg;

Theta1_grad = (Delta1/m) + p1;
Theta2_grad = (Delta2/m) + p2;


% -------------------------------------------------------------

% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end
