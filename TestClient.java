import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.Bytes;

public class TestClient {

    static String[] PHBOOK_COLS = new String[] {"cell", "homme", "mobile", "work"};

    //--------------------------------------------------------------------------------------------
    // Scanner for scanning all rows and all cols
    static void scanRows(Table table) throws IOException {
        Scan s = new Scan();
        if (true) {            
            for (String col : PHBOOK_COLS) {
                s.addColumn(Bytes.toBytes("A"), Bytes.toBytes(col));
            }    
        }
        ResultScanner rs = table.getScanner(s);
        try {
            for (Result row : rs) {
                String name = Bytes.toString(row.getRow());
                for (Cell cell : row.listCells()) {
                    String col_family = Bytes.toString(CellUtil.cloneFamily(cell));
                    String col_qualifier = Bytes.toString(CellUtil.cloneQualifier(cell));
                    String value = Bytes.toString(CellUtil.cloneValue(cell));
                    System.out.println(name + "[" + col_family + ":" + col_qualifier + "] =" + value);
                }
            }    
        } finally {
            rs.close();
        }     
    }

    //--------------------------------------------------------------------------------------------
    // Lookup a setof rows by rowkeys - rows and all cols
    static void getRows(Table table) throws IOException {
        String[] names = new String[] {"ram.varra", "priya.varra", "shreya.varra", "vamsi.varra"};
        for (String name : names) {
            Get g = new Get(Bytes.toBytes(name));
            Result r = table.get(g);
            for (String col : PHBOOK_COLS) {
                byte [] value = r.getValue(Bytes.toBytes("A"), Bytes.toBytes(col));
                String valueStr = Bytes.toString(value);
                if (valueStr != null) {
                    System.out.println(name + ":" + col + "=" + valueStr);
                }
            }
        }
    }

    //--------------------------------------------------------------------------------------------
    public static void main(String[] args) throws IOException {
        System.out.println("HBaseTestClient version 1.0");

        System.out.println("Creating HBASE Config");
        Configuration config = HBaseConfiguration.create();
        System.out.println("Created HBASE Config");

        System.out.println("Creating connection to the HBASE Cluster");
        Connection connection = ConnectionFactory.createConnection(config);
        System.out.println("Closing connection to the HBASE Cluster");
        try {
            System.out.println("Opening table");
            Table table = connection.getTable(TableName.valueOf("phbook"));
            
            System.out.println("GetROWS by rowkeys");
            getRows(table);

            System.out.println("scanRows");
            scanRows(table);
 
 
        } finally {
            connection.close();
        }
        
        System.out.println("All DONE");
    }
}