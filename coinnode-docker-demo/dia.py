from diagrams import Cluster, Diagram
from diagrams.aws.compute import EC2,ECS
from diagrams.aws.general import User

with Diagram("Simple Hschain cluster"):

    with Cluster("blockchain network"):
        with Cluster("validator nodes"):
            validators = [EC2("validator 1"),
                          EC2("validator 2"),
                          EC2("validator 3"),
                          EC2("validator 4")]
        explorer = ECS("block explorer")

    User("") - explorer - validators
