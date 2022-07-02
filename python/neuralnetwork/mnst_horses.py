import numpy
# scipy.special for the sigmoid function expit()
import scipy.special
# library for plotting arrays
#import matplotlib.pyplot
# ensure the plots are inside this notebook, not an external window matplotlib inline
import datetime
import os
import sys

# neural network class definition
class neuralNetwork:


    # initialise the neural network
    def __init__(self, inputnodes, hiddennodes, outputnodes, learningrate, act_func):
        # set number of nodes in each input, hidden, output layer
        self.inodes = inputnodes
        self.hnodes = hiddennodes
        self.onodes = outputnodes

        # link weight matrices, wih and who
        # weights inside the arrays are w_i_j, where link is from node i to node j in the next layer
        # w11 w21
        # w12 w22 etc
        self.wih = numpy.random.normal(0.0, pow(self.inodes, -0.5), (self.hnodes, self.inodes))
        self.who = numpy.random.normal(0.0, pow(self.hnodes, -0.5), (self.onodes, self.hnodes))

        #print (self.wih)
        #print (self.who)

        # learning rate
        self.lr = learningrate

        # activation function is the sigmoid function

        if act_func == 'sig' : 
           self.activation_function = lambda x: scipy.special.expit(x)
        if act_func == 'tanh' : 
           self.activation_function = lambda x : numpy.exp(x) / numpy.sum(numpy.exp(x)) # softmax
        if act_func == 'soft' : 
           self.activation_function = lambda x : x / (1 + numpy.abs(x)) # softsign

    # save neural network weights
    def save(self):
        numpy.save(self.mtype + '_' + str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_wih.npy', self.wih)
        numpy.save(self.mtype + '_' + str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_who.npy', self.who)

    # load neural network weights
    def load(self):
        fname1 = self.mtype + '_' + str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_wih.npy'
        fname2 = self.mtype + '_' + str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_who.npy'

        if os.path.exists(fname1) and os.path.exists(fname2) :
            self.wih = numpy.load(fname1)    
            self.who = numpy.load(fname2)
        else:
            print('no presets ' + fname1 + '/' + fname2)

    # train the neural network
    def train(self, inputs_list, targets_list):
        # convert inputs list to 2d array
        inputs = numpy.array(inputs_list, ndmin=2).T
        targets = numpy.array(targets_list, ndmin=2).T
        #print ('inputs', inputs)
        #print ('targets', targets)

        # calculate signals into hidden layer
        hidden_inputs = numpy.dot(self.wih, inputs)
        # calculate the signals emerging from hidden layer
        hidden_outputs = self.activation_function(hidden_inputs)

        # calculate signals into final output layer
        final_inputs = numpy.dot(self.who, hidden_outputs)
        # calculate the signals emerging from final output layer
        final_outputs = self.activation_function(final_inputs)

        # output layer error is the (target - actual)
        output_errors = targets - final_outputs
        # hidden layer error is the output_errors, split by weights, recombined at hidden nodes
        hidden_errors = numpy.dot(self.who.T, output_errors)

        #print ('output_errors', output_errors)
        #print ('hidden_errors', hidden_errors)

        # update the weights for the links between the hidden and output layers
        self.who += self.lr * numpy.dot((output_errors * final_outputs * (1.0 - final_outputs)), numpy.transpose(hidden_outputs))

        # update the weights for the links between the input and hidden layers
        self.wih += self.lr * numpy.dot((hidden_errors * hidden_outputs * (1.0 - hidden_outputs)), numpy.transpose(inputs))


    # query the neural network
    def query(self, inputs_list):
        # convert inputs list to 2d array
        inputs = numpy.array(inputs_list, ndmin=2).T

        # calculate signals into hidden layer
        hidden_inputs = numpy.dot(self.wih, inputs)
        # calculate the signals emerging from hidden layer
        hidden_outputs = self.activation_function(hidden_inputs)

        # calculate signals into final output layer
        final_inputs = numpy.dot(self.who, hidden_outputs)
        # calculate the signals emerging from final output layer
        final_outputs = self.activation_function(final_inputs)

        return final_outputs

##----------------------------------------------------------------------------

# number of input, hidden and output nodes
input_nodes = 16
hidden_nodes = 200
output_nodes = 16
mtype ='plc'
# learning rate
learning_rate = 0.1
# epochs is the number of times the training data set is used for training
epochs = 5
act_func='sig'
if len(sys.argv) >= 5 :
    if sys.argv[1] != "" : mtype = sys.argv[1]
    if sys.argv[2] != "" : learning_rate = float(sys.argv[2])
    if sys.argv[3] != "" : hidden_nodes = int(sys.argv[3])
    if sys.argv[4] != "" : epochs = int(sys.argv[4])
    if sys.argv[5] != "" : act_func = sys.argv[5]


# create instance of neural network
n = neuralNetwork(input_nodes,hidden_nodes,output_nodes, learning_rate, act_func)
n.lr = learning_rate
n.hnodes = hidden_nodes
n.epochs = epochs
n.mtype = mtype
n.act_func = act_func

# load the mnist training data CSV file into a list
training_data_file = open(os.getenv('BOT_HISTORY') + "/data/ai/nn/startprices/back/" + n.mtype + "/train_prices.csv", 'r')
training_data_list = training_data_file.readlines()
training_data_file.close()

#n.load()
# train the neural network

for e in range(epochs):
    # go through all records in the training data set
#    print('e',e, datetime.datetime.now())
    for record in training_data_list:
        # split the record by the ',' commas
        all_values = record.split(',')
        # scale and shift the inputs
        inputs = (numpy.asfarray(all_values[3:]) * 0.99) + 0.01
        # create the target output values (all 0.01, except the desired label which is 0.99)
        targets = numpy.zeros(output_nodes) + 0.01
        # all_values[0] is the target label for this record
        targets[int(all_values[0])] = 0.99
        targets[int(all_values[1])] = 0.99
        targets[int(all_values[2])] = 0.99
        n.train(inputs, targets)

# load the mnist test data CSV file into a list
test_data_file = open(os.getenv('BOT_HISTORY') + "/data/ai/nn/startprices/back/" + n.mtype + "/sample_prices.csv", 'r')
test_data_list = test_data_file.readlines()
test_data_file.close()

# test the neural network

# scorecard for how well the network performs, initially empty
scorecard = []

# go through all the records in the test data set
for record in test_data_list:
    # split the record by the ',' commas
    all_values = record.split(',')
    # correct answer is first value
    correct_label1 = int(all_values[0])
    correct_label2 = int(all_values[1])
    correct_label3 = int(all_values[2])
    # scale and shift the inputs
    inputs = (numpy.asfarray(all_values[3:])  * 0.99) + 0.01
    #print('inputs',inputs)
    # query the network
    outputs = n.query(inputs)
    #print('outputs',outputs)
    # the index of the highest value corresponds to the label
    label = numpy.argmax(outputs)
    #print('label',label,'correct_label1',correct_label1,'correct_label2',correct_label2,'correct_label3',correct_label3)

    # append correct or incorrect to list
    if label == correct_label1 or label == correct_label2 or label == correct_label3:
        # network's answer matches correct answer, add 1 to scorecard
        scorecard.append(1)
        #print('good')
    else:
        # network's answer doesn't match correct answer, add 0 to scorecard
        scorecard.append(0)
        #print('bad')


# calculate the performance score, the fraction of correct answers
scorecard_array = numpy.asarray(scorecard)
print ("performance = ", scorecard_array.sum() / scorecard_array.size, "lr=", str(n.lr), "hn=", str(n.hnodes), "ep=", str(n.epochs) , "mtype=", mtype,"act_func",n.act_func, "num tested", scorecard_array.size)

#n.save()
