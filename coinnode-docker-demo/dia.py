from diagrams import Cluster, Diagram
from diagrams.aws.compute import EC2,ECS
from diagrams.aws.general import User

with Diagram("Simple Hschain cluster"):
    with Cluster("blockchain nodes"):
        explorer = ECS("block explorer")
        with Cluster("validator nodes"):
            EC2("validator 1") - EC2("validator 2") - EC2("validator 3") - EC2("validator 4")


    User("") >> explorer
