import Models.BM25;
import Models.Document;
import Models.Query;
import Models.WorkerManager;

import java.io.File;
import java.util.ArrayList;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    static public void main(String args[]) {

        ArrayList<Document> document_list = new ArrayList<Document>();
        String path = "../../data/subset/";
        File directory = new File(path);
        File[] files = directory.listFiles();

        if (files != null) {

            WorkerManager workerManager = new WorkerManager();

            for (File file : files) {

                Runnable runnable = new Runnable() {
                    @Override
                    public void run() {
                        document_list.add(new Document(path + file.getName()));
                    }
                };
                Thread t = Thread.ofVirtual().start(runnable);

                workerManager.addWorker(t);
            }

            while (workerManager.workers_alive()) {}
        }
        else {
            System.out.println("No documents found");
        }

        System.out.println("Finished reading docs. Now calculating score...");

        BM25 bm25 = new BM25(document_list);
        String most_relevant_doc_name = bm25.get_most_relevant_doc(new Query("something"));

        System.out.println("Most relevant doc: " + most_relevant_doc_name);

    }

}