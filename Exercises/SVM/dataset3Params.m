function [C, sigma] = dataset3Params(X, y, Xval, yval)
%DATASET3PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = DATASET3PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%
test_values = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30];
combo = [[test_values; test_values]'; nchoosek(test_values, 2); fliplr(nchoosek(test_values, 2))]
C_vector = combo(:, 1);
sigma_vector = combo(:, 2);
min_error = 1;
for i = 1:length(C_vector)
  %svmTrain(X, Y, C, kernelFunction, tol, max_passes)
  model = svmTrain(X, y, C_vector(i), @(x1, x2) gaussianKernel(x1, x2, sigma_vector(i)));
  predictions = svmPredict(model, Xval);
  prediction_error = mean(double(predictions ~= yval));
  if prediction_error <= min_error
    min_error = prediction_error;
    C = C_vector(i);
    sigma = sigma_vector(i);
  endif
endfor
disp('minimum prediction error: '), disp(prediction_error);
disp('Best C parameter'), disp(C);
disp('Best sigma parameter'), disp(sigma);

% =========================================================================

end
